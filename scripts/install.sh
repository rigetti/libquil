#!/usr/bin/env bash

# -e so a failed download or unpack stops the install rather than falling through
# to copying files that were never extracted; -o pipefail so a failure on the left
# of a pipe is not masked by a successful one on the right.
set -euo pipefail

err() {
  printf "%s\n" "$@" >&2
  exit 1
}

usage() {
  cat <<'EOF'
Usage: install.sh [--prefix DIR] [--from DIR] [--install-deps] [VERSION]

Installs libquil into DIR/lib and DIR/include/libquil.

  VERSION         release to install, e.g. 0.4.0. Defaults to the latest release.
  --prefix DIR    install here instead of /usr/local. Root is needed only when the
                  prefix is not writable, so a prefix under your home directory
                  installs without sudo. Equivalent to LIBQUIL_PREFIX=DIR.
  --from DIR      install files already present in DIR instead of downloading a
                  release. Accepts either an unpacked release or a libquil build
                  tree, where the runtime lives in a runtime/ subdirectory. This
                  is what `make install` uses.
  --install-deps  also install libquil's prerequisites with apt or Homebrew.
                  Off by default: without it, missing prerequisites are reported
                  and the install stops. Equivalent to LIBQUIL_INSTALL_DEPS=1.

Consumers find a non-default prefix through LIBQUIL_SRC_PATH and LIBQUIL_LIB_PATH;
this script prints the values to use when it finishes.

Environment:
  LIBQUIL_RELEASE_REPO   repository to fetch releases from (default rigetti/libquil)
  LIBQUIL_PREFIX         install prefix (default /usr/local)
  LIBQUIL_INSTALL_DEPS   set to 1 for --install-deps
EOF
}

# Which repository to fetch releases from. Override to install from a fork, which
# is how a prerelease can be tested before it is published from the main repository.
LIBQUIL_RELEASE_REPO="${LIBQUIL_RELEASE_REPO:-rigetti/libquil}"
LIBQUIL_INSTALL_DEPS="${LIBQUIL_INSTALL_DEPS:-0}"
LIBQUIL_PREFIX="${LIBQUIL_PREFIX:-/usr/local}"
LIBQUIL_FROM=""
LIBQUIL_VERSION=""

while [[ $# -gt 0 ]]
do
  case "${1}" in
    --install-deps) LIBQUIL_INSTALL_DEPS=1 ;;
    --prefix)
      [[ -n "${2-}" ]] || err "--prefix needs a directory"
      LIBQUIL_PREFIX="${2}"
      shift
      ;;
    --prefix=*)     LIBQUIL_PREFIX="${1#--prefix=}" ;;
    --from)
      [[ -n "${2-}" ]] || err "--from needs a directory"
      LIBQUIL_FROM="${2}"
      shift
      ;;
    --from=*)       LIBQUIL_FROM="${1#--from=}" ;;
    -h | --help)    usage; exit 0 ;;
    -*)             usage >&2; err "" "Unknown option: ${1}" ;;
    *)
      if [[ -n "${LIBQUIL_VERSION}" ]]
      then
        usage >&2
        err "" "Unexpected argument: ${1}"
      fi
      LIBQUIL_VERSION="${1}"
      ;;
  esac
  shift
done

if [[ -n "${LIBQUIL_FROM}" && -n "${LIBQUIL_VERSION}" ]]
then
  err "--from installs the files in that directory, so a version cannot also be given."
fi

if [[ -n "${LIBQUIL_VERSION}" ]]
then
  LIBQUIL_URL_PREFIX="https://github.com/${LIBQUIL_RELEASE_REPO}/releases/download/v${LIBQUIL_VERSION}"
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
      # Only a problem when downloading: --from installs what is already built.
      [[ -n "${LIBQUIL_FROM}" ]] ||
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
      [[ -n "${LIBQUIL_FROM}" ]] ||
        err "Unsupported CPU architecture for macOS: ${ARCH}. Only Apple Silicon (arm64)" \
            "has published builds. You can build libquil from source; see" \
            "https://github.com/rigetti/libquil#building-from-source"
      ;;
  esac
# Windows shells report one of these. libquil publishes no Windows build, so there
# is nothing to install even where the shell would run this script.
elif [[ "${OS}" == CYGWIN* || "${OS}" == MINGW* || "${OS}" == MSYS* || "${OS}" == "Windows_NT" ]]
then
  err "Windows is not supported: libquil publishes builds for Linux and macOS only."
else
  err "Unsupported operating system: ${OS}. libquil supports Linux and macOS."
fi

if [[ -z "${LIBQUIL_FROM}" ]]
then
  for tool in curl unzip
  do
    command -v "${tool}" >/dev/null 2>&1 ||
      err "This installer needs ${tool} to download a release, and it was not found."
  done
fi

LIBQUIL_LIB_PREFIX="${LIBQUIL_PREFIX}/lib"
LIBQUIL_INCLUDE_PREFIX="${LIBQUIL_PREFIX}/include/libquil"
if [[ "${OS}" == "Darwin" ]]
then
  LIBQUIL_LD_PATH_VAR="DYLD_LIBRARY_PATH"
else
  LIBQUIL_LD_PATH_VAR="LD_LIBRARY_PATH"
fi

# Root is not a requirement of the install, only of writing to /usr/local. Test
# what is actually needed -- whether the target directories can be created and
# written -- so that `--prefix "${HOME}/.local"` works as an ordinary user.
# Checked before anything else runs so the failure is immediate.
# Create the target and write to it, rather than inspecting permission bits: -w
# disagrees with reality often enough to matter -- it ignores ACLs, reports true
# on a read-only mount, and is always true for root. The directories are needed
# either way, so making them here costs nothing.
directory_is_writable() {
  local dir="${1}" probe
  mkdir -p "${dir}" 2>/dev/null || return 1
  probe="$(mktemp "${dir}/.libquil-install-probe.XXXXXX" 2>/dev/null)" || return 1
  rm -f "${probe}"
}

for dir in "${LIBQUIL_LIB_PREFIX}" "${LIBQUIL_INCLUDE_PREFIX}"
do
  directory_is_writable "${dir}" ||
    err "Cannot write to ${dir}." \
        "Re-run with sudo, or choose a writable prefix, e.g." \
        "    install.sh --prefix \"\${HOME}/.local\""
done

# Installing prerequisites is opt-in. The default is to check and report, because
# this script is commonly run as `curl ... | sudo bash` and a package manager
# invocation there has a much wider blast radius than copying files into
# /usr/local. Consumers that want the one-shot path pass --install-deps.
install_prerequisites() {
  if [[ "${OS}" == "Darwin" ]]
  then
    command -v brew >/dev/null 2>&1 ||
      err "--install-deps needs Homebrew on macOS, which was not found." \
          "Install libquil's requirements another way and re-run without --install-deps:" \
          "https://github.com/rigetti/libquil#requirements"

    # Homebrew refuses to run as root, so it runs as whoever invoked sudo.
    # (`brew --prefix` is the one subcommand it allows as root, which is why the
    # search paths above can call it directly.)
    #
    # SUDO_USER is set only when root was reached through sudo, so this guard is
    # about not having a user to drop to -- not about Homebrew's root policy,
    # which the line below already handles. It is close to unreachable: brew is
    # not on root's default PATH, so a root login normally fails the check above
    # instead. It stays because without it `set -u` would abort here with a bare
    # "SUDO_USER: unbound variable".
    [[ -n "${SUDO_USER-}" ]] ||
      err "--install-deps needs an unprivileged user to run Homebrew as, and this is" \
          "a root session rather than one entered through sudo." \
          "Re-run it under sudo from your normal account, or install the requirements" \
          "yourself and drop --install-deps."
    local brew_cmd=(sudo -u "${SUDO_USER}" brew)

    # OpenBLAS provides both BLAS and LAPACK.
    local missing=()
    local formula
    for formula in openblas libffi
    do
      "${brew_cmd[@]}" list --formula "${formula}" >/dev/null 2>&1 || missing+=("${formula}")
    done
    if [[ "${#missing[@]}" -gt 0 ]]
    then
      echo "Installing prerequisites with Homebrew: ${missing[*]}"
      "${brew_cmd[@]}" install "${missing[@]}"
    fi
    return
  fi

  command -v apt-get >/dev/null 2>&1 ||
    err "--install-deps installs prerequisites with apt, which was not found." \
        "Install libquil's requirements with your package manager and re-run without" \
        "--install-deps: https://github.com/rigetti/libquil#requirements"

  # The -dev packages, not the runtime ones: magicl and CFFI load these under their
  # unversioned names, which only the development packages provide.
  local missing=()
  local package
  for package in libblas-dev liblapack-dev libffi-dev
  do
    if ! dpkg-query -W -f='${Status}' "${package}" 2>/dev/null | grep -q "^install ok installed$"
    then
      missing+=("${package}")
    fi
  done
  if [[ "${#missing[@]}" -gt 0 ]]
  then
    echo "Installing prerequisites with apt: ${missing[*]}"
    apt-get update
    apt-get install -y "${missing[@]}"
  fi
}

if [[ "${LIBQUIL_INSTALL_DEPS}" == "1" ]]
then
  install_prerequisites
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

# Where to look for an unversioned library, in roughly the order the platform's
# loader considers them.
LIBQUIL_SEARCH_DIRS=(/usr/local/lib /usr/lib)
if [[ "${OS}" == "Darwin" ]]
then
  if command -v brew >/dev/null 2>&1
  then
    LIBQUIL_BREW_PREFIX="$(brew --prefix)"
    LIBQUIL_SEARCH_DIRS+=("${LIBQUIL_BREW_PREFIX}/lib" "${LIBQUIL_BREW_PREFIX}/opt/openblas/lib")
  fi
else
  LIBQUIL_SEARCH_DIRS+=(/usr/lib64 /lib /lib64)
fi

library_is_available() {
  local soname="lib${1}.${LIBQUIL_LIB_SUFFIX}"

  # The loader's own cache is authoritative where it exists.
  if [[ "${OS}" != "Darwin" ]] && command -v ldconfig >/dev/null 2>&1
  then
    ldconfig -p | grep -q "[[:space:]]${soname}[[:space:]]" && return 0
  fi

  local dir
  for dir in "${LIBQUIL_SEARCH_DIRS[@]}"
  do
    [[ -e "${dir}/${soname}" ]] && return 0
  done
  return 1
}

LIBQUIL_MISSING_LIBS=()
for lib in blas lapack
do
  library_is_available "${lib}" || LIBQUIL_MISSING_LIBS+=("lib${lib}.${LIBQUIL_LIB_SUFFIX}")
done

if [[ "${#LIBQUIL_MISSING_LIBS[@]}" -gt 0 ]]
then
  err "Missing required libraries: ${LIBQUIL_MISSING_LIBS[*]}" \
      "" \
      "libquil loads these at runtime under exactly these unversioned names, so a" \
      "runtime-only package that provides a versioned name is not sufficient." \
      "" \
      "Re-run with --install-deps to install them with apt or Homebrew, or install" \
      "them yourself: https://github.com/rigetti/libquil#requirements"
fi

# The files that make up an installed libquil, named once. libquil.core has to
# land beside libsbcl_librarian, so the libraries and the core share a directory:
# the runtime locates its core relative to its own path.
LIBQUIL_HEADERS=(libquil.h sbcl_librarian.h sbcl_librarian_err.h)
LIBQUIL_LIBS=(
  "libquil.${LIBQUIL_LIB_SUFFIX}"
  "libsbcl_librarian.${LIBQUIL_LIB_SUFFIX}"
  # SBCL names its linkable runtime libsbcl.so on every platform, macOS included.
  libsbcl.so
  libquil.core
)

# A release unpacks with everything in one directory; a build tree keeps the
# runtime in runtime/. Accept both, so `make install` can hand over the tree it
# just built without staging a copy first.
locate_artifact() {
  local dir="${1}" name="${2}" candidate
  for candidate in "${dir}/${name}" "${dir}/runtime/${name}"
  do
    if [[ -f "${candidate}" ]]
    then
      printf '%s' "${candidate}"
      return 0
    fi
  done
  return 1
}

install_artifacts() {
  local source="${1}" name path
  for name in "${LIBQUIL_HEADERS[@]}"
  do
    path="$(locate_artifact "${source}" "${name}")" ||
      err "${name} is missing from ${source}."
    cp "${path}" "${LIBQUIL_INCLUDE_PREFIX}"
  done
  for name in "${LIBQUIL_LIBS[@]}"
  do
    path="$(locate_artifact "${source}" "${name}")" ||
      err "${name} is missing from ${source}."
    cp "${path}" "${LIBQUIL_LIB_PREFIX}"
  done
}

if [[ -n "${LIBQUIL_FROM}" ]]
then
  [[ -d "${LIBQUIL_FROM}" ]] || err "--from ${LIBQUIL_FROM} is not a directory."
  LIBQUIL_SOURCE_DIR="$(cd "${LIBQUIL_FROM}" && pwd)"
else
  LIBQUIL_RELEASE_URL="${LIBQUIL_URL_PREFIX}/${LIBQUIL_RELEASE_FILE}"
  LIBQUIL_TEMP_DIR="$(mktemp -d)"

  trap 'rm -rf "${LIBQUIL_TEMP_DIR}"' EXIT
  cd "${LIBQUIL_TEMP_DIR}"

  # -f so an HTTP error is a non-zero exit rather than an error page written to the
  # archive: without it a bad version tag saves a "404: Not Found" body as the .zip
  # and the failure only surfaces later, as a confusing unzip error.
  curl -fL "${LIBQUIL_RELEASE_URL}" -o "${LIBQUIL_RELEASE_FILE}" ||
    err "Could not download ${LIBQUIL_RELEASE_URL}" \
        "Check that the requested version exists: https://github.com/${LIBQUIL_RELEASE_REPO}/releases"
  unzip "${LIBQUIL_RELEASE_FILE}"
  LIBQUIL_SOURCE_DIR="${LIBQUIL_TEMP_DIR}/libquil"
fi

install_artifacts "${LIBQUIL_SOURCE_DIR}"

if [[ -n "${IS_LINUX-}" ]]
then
  # Only meaningful for a system prefix, and only permitted as root.
  if [[ "$(id -u)" -eq 0 ]]
  then
    ldconfig
  fi
else
  echo "Removing the quarantine attribute from the installed files."
  # This disables the "cannot open libquil.dylib from untrusted developer" dialog.
  # A better solution for this would be to properly codesign the files, but that
  # is a non-trivial amount of work. A no-op on files that were never quarantined,
  # such as a local build.
  for name in "${LIBQUIL_LIBS[@]}"
  do
    xattr -r -d com.apple.quarantine "${LIBQUIL_LIB_PREFIX}/${name}" 2>/dev/null || true
  done
fi

# A prefix other than /usr/local is not on any default search path, so tell the
# caller how consumers find it. libquil-sys reads both: LIBQUIL_SRC_PATH for the
# headers, LIBQUIL_LIB_PATH for the libraries, which differ because the headers
# live one directory deeper.
if [[ "${LIBQUIL_PREFIX}" != "/usr/local" ]]
then
  cat <<EOF

Installed to ${LIBQUIL_PREFIX}. That is not a default search path, so build
against it with:

    export LIBQUIL_SRC_PATH="${LIBQUIL_INCLUDE_PREFIX}"
    export LIBQUIL_LIB_PATH="${LIBQUIL_LIB_PREFIX}"

and run with:

    export ${LIBQUIL_LD_PATH_VAR}="${LIBQUIL_LIB_PREFIX}"
EOF
fi
