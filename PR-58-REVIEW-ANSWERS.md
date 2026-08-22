# PR #58 — status

Scratch notes for the author. The reasoning behind each change lives in its commit
message and in comments at the point of use; this file only tracks what is left.

## Where it stands

All review feedback has been addressed in code. Four upstream dependencies are
pinned to branches or a bare commit, each tagged with a `TODO(...)` at the pin:

| dependency | pinned to | tracked by | blocked on |
|---|---|---|---|
| sbcl-librarian | `rigetti/…:fix-secondary-system-bundles` | rigetti/libquil#60 | quil-lang/sbcl-librarian#91 merge + release |
| quilc | `rigetti/…:896-protoquil-transform` | rigetti/libquil#61 | quil-lang/quilc#933 merge |
| magicl | `rigetti/…:backend-selection-and-validation` | rigetti/libquil#62 | quil-lang/magicl#222 merge + release |
| qvm | commit `1c4ca60` | quil-lang/qvm#330 | any release newer than 1.17.2 (2021) |

`grep -rn 'TODO('` over `.github/` and `scripts/` finds all six pin lines.

**This cannot be released against stable dependencies until those four resolve.**
That is the single thing gating merge-and-release, not review.

## Still needing a decision

- **`install.sh` now requires root**, per the two comments at `install.sh:111`,
  changing `curl … | bash` to `curl … | sudo bash`. Some object to that on
  principle. The alternative floated in review — re-exec via `sudo "${0}"` — cannot
  work from a pipe, since `$0` is `bash`. Reverting to the `${SUDO}` variable is a
  small change if preferred.
- **`REARCHITECTURE.md` retained**, against "if we keep this file". It is linked
  from `README.md` and the PR description. Dropping it means fixing those two
  references.

## Not verified locally

- The **C examples** under `examples/qvm` and `examples/quilc` were never run on a
  locally built artifact: this machine's SBCL core and linkable runtime come from
  different builds, so `make` produces something that cannot load. Equivalent
  checks were run from Lisp instead. CI covers the real thing.
- The **macOS job** has not yet completed on the current head.

## Draft comment to post

---

Everything raised in review is addressed. Rather than reply inline to all 32 open
threads, a summary of the substantive ones:

**Restoring the `(ql:quickload …)` pre-flight steps** — I tried this and it breaks
the build. `src/quilc/api.lisp` deliberately redefines the `quilc_compile_protoquil`
alien callable that `define-api` generated (it needs `(* (* t))`, which
sbcl-librarian cannot express), so SBCL signals a *continuable* `redefine alien
callable` error. Under `--non-interactive` nothing takes the restart and the load
aborts — which is why `src/build-image.lisp` wraps its loads in
`with-alien-redefinition-allowed`. The validation is also redundant now: `make` runs
that file, so a dependency failure surfaces one step later with the same error.

**`libffi7` vs `libffi8`** — the two really are incompatible (the soname bumped
because `FFI_TRAMPOLINE_SIZE` grew, and it sizes the first member of the public
`ffi_closure` struct), but nothing here mixes them: `libffi-dev` on 22.04 already
resolved to `libffi.so.8`, so the explicit `libffi7` only installed a second runtime
nothing used. Also confirmed libffi *is* a real runtime dependency — it is
`dlopen`'d once Lisp is up, which is why it never shows in `otool -L`.

**The arm64 LAPACK problem** — my original framing was too broad. It is not a
property of reference LAPACK: it is a gfortran miscompile of `zlarf1f.f`, new in
LAPACK 3.12.1 (GCC PR122408 and Reference-LAPACK#1160, both fixed upstream;
Homebrew/homebrew-core#300084 tracks the un-rebuilt bottle). Confirmed by rebuilding
the bottle from source, which fixes it.

**`magicl:with-blapack`** — not a workaround and nothing to report. SBCL enables
floating-point traps by default while C and Fortran callers do not, so calling
LAPACK from Lisp surfaces exceptions no other caller sees. Adding it to
`compile-quil` to match `compile-protoquil` is a plain fix.

**Three workarounds are gone entirely**, because the fixes went upstream instead:
the vendored `PROCESS-PROTOQUIL` (quil-lang/quilc#933), the OpenBLAS preload and CI
symlinks (quil-lang/magicl#222), and — unrelated to libquil but found along the way
— a dead `init()` in sbcl-librarian (added to quil-lang/sbcl-librarian#91).

**What gates this PR** is the four dependency pins in the table above, not review.
Each has a `TODO` at the pin and an issue.

Two things I would still like a call on: `install.sh` now requires root, which makes
the documented invocation `curl … | sudo bash`; and `REARCHITECTURE.md` is still
here, which the "if we keep this file" comment left open.
