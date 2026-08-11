# libquil on modern sbcl-librarian

Working notes and decision record for the `sbcl-librarian-runtime` branch, which
moves libquil from the 2023-era sbcl-librarian (pinned at `04f7e39`) onto current
`main`. Written as the work happened; decisions are recorded with their reasons so
they can be revisited.

## Why this is not a small change

The obvious motivation was adopting sbcl-librarian's built-in error handling and
unblocking modern SBCL. Both turned out to be gated on a redesign that landed
upstream in `2a12bd0` (2024-01-23, "Add libsbcl_librarian CMake project + Conda
recipe").

Two facts establish that there is no cheaper path:

1. The `sb-int::int-sap` -> `sb-sys:int-sap` fix that modern SBCL requires landed
   in `b8dc014` (2024-05-22), *after* the redesign. There is no version window
   offering modern-SBCL compatibility with the old architecture.
2. On current `main`, the generated `init(char* core)` is dead code: it declares
   its own `static int initialized`, shadowing the global that every generated API
   wrapper checks. Calling it leaves every function returning
   `LISP_ERR_NOT_INITIALIZED`.

## The old model vs the new one

Old (what libquil does on `main`):

- `build-bindings` + `build-core-and-die` produce `libquil.c/.h` and `libquil.core`.
- `libquil.dylib` is self-contained, statically embedding the SBCL runtime.
- The consumer calls `init("/path/to/libquil.core")` explicitly.

New (what this branch moves to):

- A **runtime** is built once: `libsbcl_librarian.dylib` plus `sbcl_librarian.core`.
  It initializes Lisp from a shared-library constructor at load time, locating its
  core by name next to itself, with heap size from `SBCL_LIBRARIAN_HEAP_SIZE`.
- Each consumer is a **FASL library**: a small shared library holding the generated
  C bindings, incbin-embedded FASL bundles for its ASDF system and dependencies,
  and a constructor that loads those bundles into the already-running image.
  `create-fasl-library-cmake-project` generates the whole CMake project.
- There is no per-consumer `.core`, and no explicit init call.

## Decisions

### D1: Follow the upstream FASL-library model rather than pinning a fork

Rejected alternatives: (a) stay on `04f7e39` and carry the one-line `int-sap`
patch forever; (b) fork sbcl-librarian to keep `define-api :error-map` alive.

Both work, but they permanently diverge from upstream and keep libquil's
hand-rolled error handling, which upstream has since absorbed. Since we are
re-architecting anyway, match upstream so future updates are merges rather than
rebases.

Note (a) remains the correct *fallback* and is preserved on the
`build-apple-aarch64` branch, which builds and passes 22/22 today.

### D2: Adopt sbcl-librarian's built-in error handling; accept the ABI break

libquil's `*last-error*` / `libquil_error()` / `libquil_error_t` are replaced by
upstream's `get_error_message()` / `lisp_err_t`. This is a breaking change to
libquil's C ABI.

Accepted because libquil is pre-1.0, the only known consumer is libquil-sys (which
we control and update in lockstep), and the replacement is strictly better: it
distinguishes failure from bug from fatal, and supports backtraces.

`quilc_compile_protoquil`'s hand-written `define-alien-callable` (which exists
because sbcl-librarian cannot express `(:pointer :pointer)`) now records into
`sbcl-librarian::*error-message*` so it reports through the same channel.

### D3: Ship a core, not FASL bundles

Upstream's consumer path is `create-fasl-library-cmake-project`, which embeds a
FASL bundle per ASDF system into the library and loads them into the image at
startup. libquil was built that way first, and it worked -- but it is not
distributable:

- FASL bundles re-run load-time code on every startup. `cl-quil` computes
  `*default-standard-gates-file*` with `asdf:system-relative-pathname` and calls
  `initialize-standard-gates` inside an `eval-when (:load-toplevel)`, so every
  process that loaded libquil re-read `stdgates.quil` **from quilc's source tree**,
  at the path baked in when the bundles were built. An installed artifact on a
  machine without quilc's sources would fail.
- The same load-time chatter (`; loading standard gates from ...`) went to stdout
  of every process that linked libquil, which among other things broke
  `cargo nextest`'s test enumeration.

Saving a core evaluates all of that once, at build time. So libquil keeps a core --
what it always shipped -- and gets the modern runtime, init and error handling
around it.

The core is loaded by the runtime rather than by an explicit `init(core)` call,
which is why D6 exists.

### D4: Build the runtime manually rather than via its CMake project

Upstream's `lib/CMakeLists.txt` invokes the generator with `sbcl --script`, which
skips `~/.sbclrc` and therefore Quicklisp — so `generate-bindings.lisp`'s
`(asdf:load-system :swank)` fails with `Component :SWANK not found`.

Rather than patch upstream's CMake, `src/build-image.lisp` defines the runtime's
aggregate library itself and emits `runtime/sbcl_librarian.c` alongside libquil's
own bindings. One image therefore produces everything: libquil's bindings, the
runtime's bindings, and the core that backs both. That also avoids the swank
dependency and the second, discarded core that upstream's generator would build.

### D5: Point the runtime at libquil's core

`entry_point.c` hardcoded `sbcl_librarian.core`, looked up next to the runtime
library. Because libquil ships its own core (D3), the runtime has to load that
instead. Upstream now takes a compile-time `SBCL_LIBRARIAN_CORE_NAME`, defaulting
to the old name, and libquil builds the runtime with `-DSBCL_LIBRARIAN_CORE_NAME='"libquil.core"'`.

Naming libquil's core `sbcl_librarian.core` would have avoided the patch, but two
sbcl-librarian consumers installed into the same directory would then overwrite
each other's core.

### D6: Map ordinary errors to `LISP_ERR_FAILURE`, and handle `T`

The stock `default-error-map` classifies any `cl:error` as `LISP_ERR_BUG`, prefixes
the message with "Internal lisp bug:" and attaches a backtrace. Nearly everything
libquil signals is user error -- malformed Quil, an unknown memory region -- so
libquil redefines the map for its own callables. `wrap-error-handling` is consulted
when a callable is compiled, so this affects only libquil's; the runtime's own APIs
keep upstream behaviour.

The handler binds `T`, not `cl:error`. cl-quil signals conditions that are not
subtypes of `error` -- `invalid-instruction-condition` has no supertype at all --
and with a `cl:error` handler those escape into the debugger and hang the calling
process. libquil's original error map bound `T` for the same reason.

### D7: Keep shipping the SBCL runtime as `libsbcl.so`, even on macOS

SBCL's `make-shared-library.sh` emits `libsbcl.so` on every platform, and that
string becomes the install name recorded in `libsbcl_librarian.dylib`. Renaming the
file to `.dylib` breaks loading unless the install name is rewritten too. Upstream's
own example Makefile notes the same quirk. We ship `libsbcl.so` and leave the name
alone.

## What `make` produces

    libquil.dylib                     generated C bindings (~40 KB)
    libquil.h                         its header
    runtime/libsbcl_librarian.dylib   SBCL runtime; initializes Lisp on load
    runtime/libquil.core              the Lisp image
    runtime/libsbcl.so                the linkable SBCL runtime
    runtime/sbcl_librarian.h          runtime API (get_error_message, handles)
    runtime/sbcl_librarian_err.h      lisp_err_t and the fatal-error plumbing

All of `runtime/` must be installed together, and `libquil.core` must sit beside
`libsbcl_librarian`, since the runtime finds its core relative to its own location.

## Prerequisite: SBCL with a linkable runtime

Homebrew's bottle does not ship one, so SBCL must come from source:

    sh make.sh --with-sb-linkable-runtime && sh make-shared-library.sh && sh install.sh

## Upstream sbcl-librarian changes this depends on

Three fixes, on the `fix-secondary-system-bundles` branch:

1. **Secondary systems lost their FASL bundles.** The output-translation pattern was
   built from the flattened bundle name (`magicl--core--system.fasl`), but ASDF
   writes `magicl/core--system.fasl`, turning the slash into a directory. Bundles
   for `magicl/core`, `magicl/ext*` and `cl-quil/frontend` were silently left in the
   ASDF cache and the generated CMake project referenced files that did not exist.
   (Found while libquil was still on the FASL path; kept because it is a real bug.)
2. **`liblibquil`.** The generated CMake project cleared the library prefix only on
   Windows, so a library whose name starts with `lib` built as `liblibquil.so` on
   Unix. Upstream's own libcalc example links `-lcalc`, which only resolves with the
   prefix cleared.
3. **`SBCL_LIBRARIAN_CORE_NAME`** (D5).

## Status

- [x] Consumer model determined
- [x] Runtime builds and self-initializes on arm64 macOS
- [x] libquil builds; `make` alone produces a usable artifact
- [x] Verified from C: parse, chip, compile, program string, and error reporting
- [x] libquil-sys updated to the new ABI -- 22/22 tests pass
- [x] Python bindings rebuilt and exercised
- [x] C examples updated and passing (`make test` in both `examples/` directories)
- [x] CI, `install.sh` and release archives updated for the new artifact set
- [x] Packaging verified by simulating package -> zip -> install -> build and
      running the libquil-sys suite against the installed layout (22/22)

## Open

**CI depends on a fork branch.** `build.yml` clones `$SBCL_LIBRARIAN_REPO` at
`$SBCL_LIBRARIAN_REF`, currently `rigetti/sbcl-librarian` at
`fix-secondary-system-bundles`. Move it back to `quil-lang/sbcl-librarian` once the
three fixes are upstream.

**CI's SBCL bump is unverified.** Both jobs now build `sbcl-$SBCL_VERSION` (2.6.7)
with `--with-sb-linkable-runtime`, replacing the 2.2.4 build that failed in
`make-host-1`. That combination is proven locally on arm64 macOS but has not run on
the Linux job.

**Consumers with a non-/usr/local prefix** need `LIBQUIL_LIB_PATH` as well as
`LIBQUIL_SRC_PATH`, since headers and libraries then live in different directories.

**Upstreaming.** The three sbcl-librarian fixes are worth PRs regardless of what
libquil does; the FASL-bundle one is a plain bug, and the library-prefix one breaks
upstream's own libcalc example on Unix.
