#!/bin/bash

set -u

err() {
  printf "%s\n" "$@" >&2
  exit 1
}

# Which repository to fetch releases from. Override to install from a fork, which
# is how a prerelease can be tested before it is published from the main repository.
LIBQUIL_RELEASE_REPO="${LIBQUIL_RELEASE_REPO:-rigetti/libquil}"

if [[ -n "${1-}" ]]
then
  LIBQUIL_URL_PREFIX="https://github.com/${LIBQUIL_RELEASE_REPO}/releases/download/v${1}"
else
  LIBQUIL_URL_PREFIX="https://github.com/${LIBQUIL_RELEASE_REPO}/releases/latest/download"
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

# magicl dlopens BLAS and LAPACK under their unversioned names once libquil is in use,
# so a missing one is not a link error at install time but a failure much later, in the
# middle of compiling a program. Check for them up front instead.
if [[ "${OS}" == "Darwin" ]]
then
  LIBQUIL_LIB_SUFFIX="dylib"
else
  LIBQUIL_LIB_SUFFIX="so"
fi

library_is_available() {
  local soname="lib${1}.${LIBQUIL_LIB_SUFFIX}"

  # The loader's own cache is authoritative where it exists.
  if [[ -z "${IS_LINUX-}" ]]
  then
    # dyld has no queryable cache; check the paths it searches by default, plus the
    # Homebrew prefixes that are not on it. Counting the latter keeps a normal
    # `brew install openblas` from being reported as missing, at the cost of not
    # catching the case where magicl ends up unable to load a keg-only install.
    local dir
    for dir in /usr/local/lib /usr/lib /opt/homebrew/lib /opt/homebrew/opt/openblas/lib
    do
      [[ -e "${dir}/${soname}" ]] && return 0
    done
    return 1
  elif command -v ldconfig >/dev/null 2>&1
  then
    ldconfig -p | grep -q "[[:space:]]${soname}[[:space:]]" && return 0
  fi

  local dir
  for dir in /usr/local/lib /usr/lib /usr/lib64 /lib /lib64
  do
    [[ -e "${dir}/${soname}" ]] && return 0
  done
  return 1
}

LIBQUIL_MISSING=()
for lib in blas lapack
do
  library_is_available "${lib}" || LIBQUIL_MISSING+=("lib${lib}.${LIBQUIL_LIB_SUFFIX}")
done

if [[ "${#LIBQUIL_MISSING[@]}" -gt 0 ]]
then
  err "Missing required libraries: ${LIBQUIL_MISSING[*]}" \
      "" \
      "libquil loads these at runtime under exactly these unversioned names, so a" \
      "runtime-only package that provides a versioned name is not sufficient." \
      "See https://github.com/rigetti/libquil#requirements"
fi

LIBQUIL_RELEASE_URL="${LIBQUIL_URL_PREFIX}/${LIBQUIL_RELEASE_FILE}"
LIBQUIL_TEMP_DIR="$(mktemp -d)"
LIBQUIL_LIB_PREFIX="/usr/local/lib"
LIBQUIL_INCLUDE_PREFIX="/usr/local/include/libquil"

# Installing into /usr/local needs root. Container images commonly run as root without
# sudo installed, where calling it would fail even though nothing needs elevating.
if [[ "$(id -u)" -eq 0 ]]
then
  SUDO=""
elif command -v sudo >/dev/null 2>&1
then
  SUDO="sudo"
else
  err "This installer needs root to write to ${LIBQUIL_LIB_PREFIX} and ${LIBQUIL_INCLUDE_PREFIX}," \
      "but it is not running as root and sudo is not available."
fi

pushd "${LIBQUIL_TEMP_DIR}" || exit
curl -L "${LIBQUIL_RELEASE_URL}" -o "${LIBQUIL_RELEASE_FILE}"
unzip "${LIBQUIL_RELEASE_FILE}"

# libquil.core must land in the same directory as libsbcl_librarian: the runtime
# locates its core relative to its own path.
${SUDO} mkdir -p "${LIBQUIL_LIB_PREFIX}" "${LIBQUIL_INCLUDE_PREFIX}"
${SUDO} cp libquil/libquil.h libquil/sbcl_librarian.h libquil/sbcl_librarian_err.h "${LIBQUIL_INCLUDE_PREFIX}"
${SUDO} cp libquil/libquil.core libquil/libsbcl.so "${LIBQUIL_LIB_PREFIX}"

if [[ -n "${IS_LINUX-}" ]]
then
  ${SUDO} cp libquil/libquil.so libquil/libsbcl_librarian.so "${LIBQUIL_LIB_PREFIX}"
  ${SUDO} ldconfig
else
  ${SUDO} cp libquil/libquil.dylib libquil/libsbcl_librarian.dylib "${LIBQUIL_LIB_PREFIX}"
  # This disables the "cannot open libquil.dylib from untrusted developer" dialog.
  # A better solution for this would be to properly codesign the files, but that
  # is a non-trivial amount of work.
  ${SUDO} xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libquil.dylib"
  ${SUDO} xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libsbcl_librarian.dylib"
  ${SUDO} xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libquil.core"
  ${SUDO} xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/libsbcl.so"
fi
