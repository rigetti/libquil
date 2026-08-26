#!/usr/bin/env bash
# Build and install an SBCL with a linkable runtime (libsbcl.so), then install
# Quicklisp. Shared by the Linux and macOS build jobs, which otherwise differ only
# in how the cross-compilation host is provided.
#
# A linkable runtime is required and `make.sh` does not build one by default, nor
# do the binary releases or the apt/Homebrew packages ship one, so SBCL has to be
# built from source. The host SBCL must already be on PATH and of roughly the same
# vintage as SBCL_VERSION.
#
# Required environment:
#   SBCL_VERSION           SBCL release to build, e.g. 2.6.7
#   QUICKLISP_VERSION      Quicklisp dist to pin, e.g. 2026-01-01
#   GITHUB_WORKSPACE       added to ql:*local-project-directories*
# Optional environment:
#   SBCL_SRC_DIR           where to clone SBCL (default /tmp/sbcl)
#   SBCL_REMOVE_HOST_CMD   run once the build is done and the host is no longer
#                          needed, to get it out of the way before installing

set -euo pipefail

: "${SBCL_VERSION:?SBCL_VERSION must be set}"
: "${QUICKLISP_VERSION:?QUICKLISP_VERSION must be set}"
: "${GITHUB_WORKSPACE:?GITHUB_WORKSPACE must be set}"

SBCL_SRC_DIR="${SBCL_SRC_DIR:-/tmp/sbcl}"

sudo git clone --single-branch --branch "sbcl-${SBCL_VERSION}" \
  https://git.code.sf.net/p/sbcl/sbcl "${SBCL_SRC_DIR}"
cd "${SBCL_SRC_DIR}"
sudo sh make.sh --with-sb-linkable-runtime
sudo sh make-shared-library.sh

if [[ -n "${SBCL_REMOVE_HOST_CMD:-}" ]]
then
  eval "${SBCL_REMOVE_HOST_CMD}"
fi

sudo sh install.sh
sudo mkdir -p /usr/local/lib
sudo cp src/runtime/libsbcl.so /usr/local/lib/libsbcl.so

if [[ ! -f ~/quicklisp/setup.lisp ]]
then
  wget -q -P /tmp/ 'https://beta.quicklisp.org/quicklisp.lisp'
  sbcl --noinform --non-interactive --load /tmp/quicklisp.lisp \
    --eval "(quicklisp-quickstart:install :dist-url \"http://beta.quicklisp.org/dist/quicklisp/${QUICKLISP_VERSION}/distinfo.txt\")"
  sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))'
  rm -f /tmp/quicklisp.lisp
fi

echo "#+quicklisp(push (truename \"${GITHUB_WORKSPACE}\") ql:*local-project-directories*)" >> ~/.sbclrc
cat ~/.sbclrc
