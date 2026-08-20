# Answers to review comments on #58

Companion to the code changes made in response to review. Comments that were
straightforward fixes are listed under **Applied**; everything that was a question,
an uncertainty, or a decision worth recording is answered below.

## Questions answered

### `(ql:quickload ...)` and Quicklisp (`.github/workflows/build.yml:66`)

Yes — Quicklisp is the package manager, ASDF is the build system underneath it (see
above). `(ql:quickload :libquil)` fetches the dependency closure and compiles and
loads the system into the running image.

### Should the deleted `(ql:quickload ...)` validation steps be restored? (`build.yml`)

**Recommendation: no, and the naive restore would break the build.** The two lines
that were removed were:

```
sbcl --noinform --non-interactive --eval '(ql:quickload :sbcl-librarian)'
sbcl --dynamic-space-size 8192 --noinform --non-interactive --eval '(ql:quickload :libquil)'
```

Loading `:libquil` on its own now signals a *continuable* error. `src/quilc/api.lisp`
deliberately redefines the `quilc_compile_protoquil` alien callable that
sbcl-librarian's `define-api` generated a few forms earlier (it needs
`(* (* t))`, which sbcl-librarian cannot express). SBCL signals
`redefine alien callable` for that and offers a `continue` restart. Under
`--non-interactive` there is nothing to take the restart, so the load aborts.

That is exactly why `src/build-image.lisp` wraps every load in
`with-alien-redefinition-allowed`. A bare `ql:quickload :libquil` outside that
handler is not a valid way to load libquil any more.

The validation those lines provided is also now redundant: `make` runs
`src/build-image.lisp`, which quickloads `sbcl-librarian` and then `libquil`, so a
dependency-resolution failure fails the `Build libquil.so` step with the same error
it would have produced earlier — just one step later.

If a separate pre-flight check is still wanted, it has to install the same handler,
e.g.:

```
sbcl --noinform --non-interactive \
  --eval '(handler-bind ((error (lambda (c) (let ((r (find-restart (quote continue) c))) (when (and r (search "redefine alien callable" (princ-to-string c))) (invoke-restart r)))))) (ql:quickload :libquil))'
```

which is the build step with extra steps. Left out.

### Are `libffi` 7 and 8 both supported? (`build.yml:96`)

They are genuinely incompatible, but nothing here mixes them, so dropping the
explicit `libffi7` is safe — and the old entry was already doing nothing.

Debian/Ubuntu's `libffi7`/`libffi8` package names track the shared object's *soname*
(`libffi.so.7` / `libffi.so.8`), not an upstream libffi version — upstream has been on
3.x since 2008. `libtool-version` went `8:0:1` (3.3) to `9:0:1` (3.4), and libtool's
soname is `current - age`, giving 7 and 8 respectively.

The bump was warranted. libffi commit
[`7855656`](https://github.com/libffi/libffi/commit/7855656148b96c7070ec362d2a73af840025a2b7)
("x86: Add indirect branch tracking support", 2020-02-21) says:

> Trampoline must be enlarged to add ENDBR instruction unconditionally, which is NOP
> on non-CET processors. [...] Update library version for larger `FFI_TRAMPOLINE_SIZE`.

`FFI_TRAMPOLINE_SIZE` went 24 -> 32 on x86-64 and 12 -> 16 on i386, and it sizes the
first member of the *public* `ffi_closure` struct in `ffi.h`:

```c
typedef struct {
  char tramp[FFI_TRAMPOLINE_SIZE];
  ffi_cif   *cif;
  void     (*fun)(ffi_cif*,void*,void**,void*);
  void      *user_data;
} ffi_closure;
```

So a binary compiled against 3.3's header allocates a 24-byte trampoline and expects
`cif` at offset 24, while 3.4 writes 32 bytes and reads `cif` at offset 32 — an
out-of-bounds write and garbage field reads, not a clean load failure. The soname
bump is what prevents the loader from ever allowing that.

None of which is reachable here. `libffi-dev` on ubuntu-22.04 resolves `libffi.so` to
`libffi.so.8`, so the build linked against 8 whether or not `libffi7` was in the apt
list — the explicit `libffi7` never influenced what was linked, it just installed a
second runtime nothing used. Naming a version is also what actually breaks the
workflow: `libffi7` does not exist on Ubuntu 24.04. The package list no longer names
one, and the explanatory comment has been shortened to one line.

**Verified: libffi is a real runtime dependency.** I initially guessed it might be
vestigial, on the grounds that SBCL's `sb-alien` does not use libffi and `libquil.asd`
depends on `#:cffi` rather than `#:cffi-libffi`. That was wrong. Tracing a run of
`examples/quilc/version` against the released 0.4.0-rc.0 artifact with
`DYLD_PRINT_LIBRARIES`, and diffing against a trivial C program as a baseline, the
objects libquil adds are:

    libopenblasp-r0.3.34.dylib                       magicl's backend (confirms D7)
    libgfortran / libquadmath / libgcc_s / libomp    OpenBLAS's dependencies
    libzstd.1.5.7.dylib                              core decompression
    /usr/lib/libffi.dylib                            dlopen'd at runtime
    libquil / libsbcl_librarian / libsbcl

libffi does not appear in `otool -L` on any shipped object, which is why it looked
absent -- it is loaded dynamically once Lisp is up. So `libffi-dev` belongs in the
requirements, and it is needed at *run* time, not only to build.

The same trace independently confirms D7: OpenBLAS is what magicl loads. Accelerate's
`libBLAS`/`libLAPACK` do appear, but they appear in the baseline too, so they are
system-framework noise rather than magicl's choice.

**Follow-up this raises.** On Linux, CFFI resolves libffi under its unversioned name,
exactly as magicl does for BLAS/LAPACK -- so the argument for `install.sh`'s preflight
check applies to libffi too. I have not added it, because whether the existing check
would find it on Debian is untested: the unversioned `libffi.so` lives in the
multiarch directory (`/usr/lib/x86_64-linux-gnu`), which is not in
`LIBQUIL_SEARCH_DIRS`, and I have not confirmed that `ldconfig -p` lists the
unversioned dev symlink. Worth settling on a Linux box before extending the loop.

### Is there a canonical way to search all dynamic-library paths on macOS? (`install.sh:72`)

No, and the options each answer a slightly different question than the one the
installer needs:

- **`man dyld`** — the default search is `/usr/local/lib` then `/usr/lib`, plus
  `DYLD_LIBRARY_PATH` / `DYLD_FALLBACK_LIBRARY_PATH` if set. Those two directories
  are now the base of the search list.
- **`DYLD_*` environment variables** — deliberately ignored. They apply to the
  installing shell, not to whatever process later loads libquil, so honouring them
  would make the check pass for installs the eventual consumer cannot see.
- **`otool -L`** — lists what a binary is *already* linked against. `magicl` `dlopen`s
  BLAS/LAPACK at runtime rather than linking them, so they never appear.
- **`find / -name`** — exhaustive but slow enough to be hostile in an installer, and
  a hit outside a loader path is not actually usable.
- **`locate`** — needs a database most machines have never built (as you found).
- **`pkg-config`** — answers "is there a `.pc` file for this?", which is not the
  question. Homebrew's `openblas` is keg-only and its file is `openblas.pc`, not
  `blas.pc`/`lapack.pc`, so `pkg-config --exists blas` fails on exactly the machine
  the check most needs to pass. What matters here is whether an *unversioned*
  `libblas.dylib` / `liblapack.dylib` exists somewhere the loader will find it,
  which is a file-existence test.

So the check stays a directory scan, but it is now built from a single
`LIBQUIL_SEARCH_DIRS` array, with the Homebrew prefixes discovered via
`brew --prefix` rather than hardcoded, and the Linux-only directories appended in
the Linux branch. That also removes the duplication noted at `install.sh:87`.

`pkg-config` remains a requirement for the *build* (zstd), which is a separate
concern.

### What about OpenBLAS? (`install.sh:94`)

The loop checks `blas` and `lapack`, which is correct: `magicl` `dlopen`s
`libblas`/`liblapack` under those names. OpenBLAS satisfies both — on macOS via the
symlinks CI creates, on Linux via the distribution's alternatives mechanism — so
checking for `libopenblas` directly would reject a perfectly good reference-BLAS
install and miss nothing.

The confusingly named `LIBQUIL_MISSING` has been renamed `LIBQUIL_MISSING_LIBS`.

### Is `sbcl_librarian.core` generated or not? (`Makefile:66`)

Both statements were true and the comment was the confusing part. `SBCL_LIBRARIAN_CORE_NAME`
is a *compile-time* setting on the runtime: it is the filename the runtime looks for
next to itself at load time, and it defaults to `sbcl_librarian.core`. libquil never
produces a file by that name — the build emits `libquil_core.core` and renames it to
`runtime/libquil.core` — so the runtime is compiled with the default overridden. The
comment now says that.

## Items that looked like upstream bug reports

Three claims in the diff read as latent upstream bugs. All three are real; two were
already reported and fixed upstream before we got here, and the third still stands.
None of them block this PR.

1. **Homebrew reference LAPACK computes incorrect eigenvectors on arm64.**
   (`build.yml`, `README.md`, `REARCHITECTURE.md` D7.) Real, reproduced, and fully
   diagnosed upstream — but the diff's framing is too broad. This is not a property
   of reference LAPACK on arm64. It is a **gfortran miscompile of LAPACK >= 3.12.1**:
   gfortran >= 15.2 auto-vectorizes `TAU * DCONJG(WORK(I))` in the new `zlarf1f.f`
   into aarch64 `fcmla` and drops the conjugation
   ([Reference-LAPACK#1160](https://github.com/Reference-LAPACK/lapack/issues/1160),
   [GCC PR122408](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=122408), fixed by
   `c5fa3d4c8` on 2025-10-27).

   Confirmed end to end on an M-series machine: Homebrew's poured `lapack` 3.12.1
   bottle gives residuals of order 1 for `zgeev`/`zheev`/`zgesvd` at n>=3 while real
   routines stay at 1e-16; `brew reinstall --build-from-source lapack` with the fixed
   gfortran drops every one of them to ~1e-15. The shipped `_zlarf1f_` contains
   `fcmla ... #90` where the rebuilt one has `#270`.

   Nothing was left to report to LAPACK or GCC — both are already fixed. The
   remaining gap was that Homebrew had not bumped `revision` to rebuild the affected
   bottle, and that is now filed as
   **[Homebrew/homebrew-core#300084](https://github.com/Homebrew/homebrew-core/issues/300084)**,
   with the standalone reproducer at
   [BatmanAoD/lapack-arm64-homebrew-bugs](https://github.com/BatmanAoD/lapack-arm64-homebrew-bugs).

   D7 remains correct regardless, because its other half — Accelerate's missing
   `zuncsd_` — is permanent. Only the *justification* in the code comment needs
   narrowing.

2. **macOS LAPACK raises division-by-zero traps.** (`src/quilc/compile.lisp:75`.)
   Two separate things are going on, and they pull in opposite directions.

   **The mask is required regardless, and is not a workaround.** SBCL enables
   floating-point traps by default — `(:overflow :invalid :divide-by-zero)` — while
   C and Fortran callers normally run with them masked. LAPACK is written on the
   assumption of the latter, so calling it from Lisp surfaces exceptions that no
   other caller ever sees. `magicl:with-blapack` is just
   `sb-int:with-float-traps-masked (:divide-by-zero :invalid)`, which restores the
   environment LAPACK expects. Adding it to `compile-quil` to match
   `compile-protoquil` is a plain correctness fix, and nothing about it belongs
   upstream in magicl — magicl already provides exactly this.

   **But the exceptions themselves are recognised bugs, and are being fixed.** In
   [Reference-LAPACK#1282](https://github.com/Reference-LAPACK/lapack/issues/1282)
   Intel's OneMKL team reported crashes under `-ffpe-trap`/`-fpe0`, and
   [PR #1283](https://github.com/Reference-LAPACK/lapack/pull/1283) was merged on
   2026-05-21 fixing them across ten files (`?gedmd`, `?gejsv`, `?gesvdx`,
   `?gesvj`). The unguarded `OFL / ROOTSC` in `dgedmd` now reads
   `IF ( ROOTSC .GT. ONE ) TBIG = OFL / ROOTSC`.

   So a spurious FPE in a LAPACK routine *is* reportable, and upstream will act on
   it — though not unanimously: a maintainer noted on that issue that "generating
   `Inf` and working with `Inf` is part of the game", while accepting the fix
   anyway.

   **What this means here.** The mask stays, permanently — it is about SBCL's
   defaults, not about LAPACK's bugs, and every LAPACK in the wild predates PR #1283 in any case. But if it is ever worth knowing *which* routine traps in
   `compile-quil`, that is a reportable upstream bug of exactly the class just
   fixed, not something to be resigned to.

   *(I previously wrote that these exceptions are "by design" and that there was
   nothing to report. That was wrong: #1282 was closed as completed because the
   fixes were merged.)*

3. **`init(char* core)` is dead code on sbcl-librarian `main`.**
   (`REARCHITECTURE.md`, "Why this is not a small change".) The generated function
   declares its own `static int initialized`, shadowing the global that every
   generated wrapper tests, so calling it leaves every function returning
   `LISP_ERR_NOT_INITIALIZED`. Worth an upstream issue independent of libquil.

The three sbcl-librarian fixes libquil depends on are already open as
[quil-lang/sbcl-librarian#91](https://github.com/quil-lang/sbcl-librarian/pull/91);
`build.yml`'s `SBCL_LIBRARIAN_REPO` comment now links to it.

## Applied without further comment

- `build.yml`: the `2022-04-01` Quicklisp-pin aside and the `sbcl-2.2.4` aside
  removed; the `libffi7` rationale cut to one line; `SBCL_LIBRARIAN_REPO` now links
  the tracking PR; `env.ImageOS` explained in both cache steps.
- `build.yml`: the duplicated SBCL-and-Quicklisp install and Lisp-dependency clone
  are now `scripts/install-sbcl.sh` and `scripts/clone-lisp-deps.sh`, shared by both
  jobs. The platform-specific part (apt bootstrap host vs. Homebrew host) stays in
  the workflow; the macOS job passes `SBCL_REMOVE_HOST_CMD` to drop the Homebrew
  `sbcl` once the build no longer needs it.
- `release.yml`: the "two macOS artifacts" comment reworded (there is only one macOS
  artifact now); the knope-version rationale comment deleted.
- `examples/quilc/Makefile`: the note about the removed `-pagezero_size` deleted.
  Confirmed: the Lisp image is mapped by `libsbcl_librarian`, not by the example
  executable, so the example no longer needs its own zero page moved.
- `src/libquil.lisp`: comment suggestion applied.
- `install.sh`: the loader-cache comment moved onto the `ldconfig` branch it
  describes; the platform test switched from `-z "${IS_LINUX-}"` to an explicit
  `"${OS}" != "Darwin"`; search paths unified into one array; `sudo` handling
  replaced with a hard root check; an `echo` added before the quarantine removal.
- `README.md`: says the `-dev` packages are what is needed; "any package manager"
  narrowed to the two actually checked; install commands updated for the root
  requirement (`curl … | sudo bash`).
- `README.md`: the C API reference still documented the removed
  `libquil_error_t`/`libquil_error()`. Updated to `lisp_err_t` /
  `get_error_message()` / `enable_backtrace()` per D2. (Not raised in review, but
  wrong as it stood.)
- `REARCHITECTURE.md`: "upstream" in D4 disambiguated to sbcl-librarian; the
  point-in-time `## Status` checklist removed. `## Open` was kept, retitled
  `## Known limitations and follow-ups`, since it is the only record of the fork
  dependency and the release ordering.

## Open decisions for you

- **`install.sh` now requires root**, per the two comments at `install.sh:111`. That
  changes the documented invocation from `curl … | bash` to `curl … | sudo bash`,
  which some people object to on principle. The alternative you floated —
  re-exec via `sudo "${0}"` — does not work when the script is piped from `curl`,
  since `$0` is `bash`. Say the word and I will restore the `${SUDO}` variable
  instead.
- **`REARCHITECTURE.md` retained.** The comment at line 216 said "if we keep this
  file". It is currently linked from `README.md` and from the PR description, so it
  is kept; if you would rather it go, `README.md`'s "Building from source" section
  and the D-numbered references in this document are the things to fix up.
- **The knope-version comment** at `release.yml:63` was deleted whole rather than
  just its last line — the empty suggestion landed on the final line of a
  three-line sentence, and deleting only that line left broken prose.
