#!/usr/bin/env bash
# Clone the Lisp systems libquil is built against into GITHUB_WORKSPACE, which the
# build adds to ql:*local-project-directories*. Shared by the Linux and macOS jobs.
#
# Required environment:
#   GITHUB_WORKSPACE       where the systems are cloned
#   SBCL_LIBRARIAN_REPO    sbcl-librarian remote to clone
#   SBCL_LIBRARIAN_REF     ref to check out in it

set -euo pipefail

: "${GITHUB_WORKSPACE:?GITHUB_WORKSPACE must be set}"
: "${SBCL_LIBRARIAN_REPO:?SBCL_LIBRARIAN_REPO must be set}"
: "${SBCL_LIBRARIAN_REF:?SBCL_LIBRARIAN_REF must be set}"
: "${MAGICL_REPO:?MAGICL_REPO must be set}"
: "${MAGICL_REF:?MAGICL_REF must be set}"
: "${QUILC_REPO:?QUILC_REPO must be set}"
: "${QUILC_REF:?QUILC_REF must be set}"

# quil-lang/qvm has had no release since 1.17.2 (2021), so this is a commit
# rather than a tag. Currently the head of master, which is five commits ahead
# of the 2022 revision libquil pinned when CI was first written; those include
# the move to clos-encounters, which current cl-quil also uses.
QVM_REF="1c4ca60d7912219063e3969be92ec45e12a0798f"

git clone "${QUILC_REPO}" "${GITHUB_WORKSPACE}/quilc"
git -C "${GITHUB_WORKSPACE}/quilc" checkout "${QUILC_REF}"

git clone https://github.com/quil-lang/qvm.git "${GITHUB_WORKSPACE}/qvm"
git -C "${GITHUB_WORKSPACE}/qvm" checkout "${QVM_REF}"

git clone "${MAGICL_REPO}" "${GITHUB_WORKSPACE}/magicl"
git -C "${GITHUB_WORKSPACE}/magicl" checkout "${MAGICL_REF}"
git clone https://github.com/stylewarning/cl-permutation "${GITHUB_WORKSPACE}/cl-permutation"

git clone "${SBCL_LIBRARIAN_REPO}" "${GITHUB_WORKSPACE}/sbcl-librarian"
git -C "${GITHUB_WORKSPACE}/sbcl-librarian" checkout "${SBCL_LIBRARIAN_REF}"
