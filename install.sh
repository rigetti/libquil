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
if [[ "${OS}" == "Linux" ]]
then
  IS_LINUX=1
  LIBQUIL_RELEASE_FILE="linux-amd64.zip"
elif [[ "${OS}" == "Darwin" ]]
then
  LIBQUIL_RELEASE_FILE="macos.zip"
else
  err "Unsupported operating system. Supported operating systems are Linux and macOS."
fi

LIBQUIL_RELEASE_URL="${LIBQUIL_URL_PREFIX}/${LIBQUIL_RELEASE_FILE}"
LIBQUIL_TEMP_DIR="$(mktemp -d)"
LIBQUIL_LIB_PREFIX="/usr/local/lib"
LIBQUIL_INCLUDE_PREFIX="/usr/local/include/libquil"

echo "${LIBQUIL_TEMP_DIR}"
pushd "${LIBQUIL_TEMP_DIR}" || exit
curl -L "${LIBQUIL_RELEASE_URL}" -o "${LIBQUIL_RELEASE_FILE}"
unzip "${LIBQUIL_RELEASE_FILE}"

if [[ -n "${IS_LINUX-}" ]]
then
  sudo cp libquil/libquil.so libquil/libquil.core libquil/libsbcl.so "${LIBQUIL_LIB_PREFIX}" 
  sudo mkdir -p "${LIBQUIL_INCLUDE_PREFIX}"
  sudo cp libquil/libquil.h "${LIBQUIL_INCLUDE_PREFIX}"
else
  sudo cp libquil/libquil.dylib libquil/libquil.core libquil/libsbcl.so "${LIBQUIL_LIB_PREFIX}" 
  sudo mkdir -p "${LIBQUIL_INCLUDE_PREFIX}"
  sudo cp libquil/libquil.h "${LIBQUIL_INCLUDE_PREFIX}"
  sudo xattr -r -d com.apple.quarantine /usr/local/lib/libquil.dylib
  sudo xattr -r -d com.apple.quarantine /usr/local/lib/libquil.core
  sudo xattr -r -d com.apple.quarantine /usr/local/lib/libsbcl.so
fi
