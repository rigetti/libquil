# PR #58 — status

Scratch notes for the author. The reasoning behind each change lives in its commit
message and in comments at the point of use; this file only tracks what is left.

## Still needing a decision

- **`REARCHITECTURE.md` retained**, against "if we keep this file". It is linked
  from `README.md` and the PR description. Dropping it means fixing those two
  references.

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

**Three workarounds are gone entirely**, because the fixes went upstream instead:
the vendored `PROCESS-PROTOQUIL` (quil-lang/quilc#933), the OpenBLAS preload and CI
symlinks (quil-lang/magicl#222), and — unrelated to libquil but found along the way
— a dead `init()` in sbcl-librarian (added to quil-lang/sbcl-librarian#91).

CI is green on both platforms, including the C example tests for quilc and qvm and
a from-source SBCL build on macOS.

**What gates this PR** is not review but four dependency pins, each tagged with a
`TODO(...)` where it is pinned:

| dependency | tracked by | blocked on |
|---|---|---|
| sbcl-librarian | #60 | quil-lang/sbcl-librarian#91 merge + release |
| quilc | #61 | quil-lang/quilc#933 merge |
| magicl | #62 | quil-lang/magicl#222 merge + release |
| qvm | quil-lang/qvm#330 | any release newer than 1.17.2 (2021) |

This cannot be released against stable dependencies until those resolve.

Two things I would still like a call on: `install.sh` now requires root, which makes
the documented invocation `curl … | sudo bash`; and `REARCHITECTURE.md` is still
here, which the "if we keep this file" comment left open.
