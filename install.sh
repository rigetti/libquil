#!/bin/bash

set -u

err() {
  printf "%s\n" "$@" >&2
  exit 1
}

if [[ -n "${1-}" ]]
then
  LIBQUIL_URL_PREFIX="https://github.com/rigetti/libquil/releases/download/v${1}"
else
  LIBQUIL_URL_PREFIX="https://github.com/rigetti/libquil/releases/latest/download"
fi

OS="$(uname)"
ARCH="$(uname -m)"
if [[ "${OS}" == "Linux" ]]
then
  IS_LINUX=1
  case "${ARCH}" in
    x86_64 | amd64)
      LIBQUIL_RELEASE_FILE="linux-amd64.zip"
      ;;
    *)
      err "Unsupported CPU architecture for Linux: ${ARCH}. Only x86_64 is supported." \
          "You can build libquil from source; see https://github.com/rigetti/libquil#building-from-source"
      ;;
  esac
elif [[ "${OS}" == "Darwin" ]]
then
  case "${ARCH}" in
    arm64 | aarch64)
      LIBQUIL_RELEASE_FILE="macos-arm64.zip"
      ;;
    *)
      err "Unsupported CPU architecture for macOS: ${ARCH}. Only Apple Silicon (arm64) is supported." \
          "Intel macOS builds are no longer published. You can build libquil from source; see" \
          "https://github.com/rigetti/libquil#building-from-source"
      ;;
  esac
else
  err "Unsupported operating system. Supported operating systems are Linux and macOS."
fi

LIBQUIL_RELEASE_URL="${LIBQUIL_URL_PREFIX}/${LIBQUIL_RELEASE_FILE}"
LIBQUIL_TEMP_DIR="$(mktemp -d)"
LIBQUIL_LIB_PREFIX="/usr/local/lib"
LIBQUIL_INCLUDE_PREFIX="/usr/local/include/libquil"

pushd "${LIBQUIL_TEMP_DIR}" || exit
curl -L "${LIBQUIL_RELEASE_URL}" -o "${LIBQUIL_RELEASE_FILE}"
unzip "${LIBQUIL_RELEASE_FILE}"

# libquil.core must land in the same directory as libsbcl_librarian: the runtime
# locates its core relative to its own path.
sudo mkdir -p "${LIBQUIL_INCLUDE_PREFIX}"
sudo cp libquil/libquil.h libquil/sbcl_librarian.h libquil/sbcl_librarian_err.h "${LIBQUIL_INCLUDE_PREFIX}"
sudo cp libquil/libquil.core libquil/libsbcl.so "${LIBQUIL_LIB_PREFIX}"

if [[ -n "${IS_LINUX-}" ]]
then
  sudo cp libquil/libquil.so libquil/libsbcl_librarian.so "${LIBQUIL_LIB_PREFIX}"
  sudo ldconfig
else
  sudo cp libquil/libquil.dylib libquil/libsbcl_librarian.dylib "${LIBQUIL_LIB_PREFIX}"
  # This disables the "cannot open libquil.dylib from untrusted developer" dialog.
  # A better solution for this would be to properly codesign the files, but that
  # is a non-trivial amount of work.
  sudo xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libquil.dylib"
  sudo xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libsbcl_librarian.dylib"
  sudo xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libquil.core"
  sudo xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libsbcl.so"
fi
