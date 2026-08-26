#!/usr/bin/env bash
# Assemble the directory that becomes a release archive. Shared by the Linux and
# macOS build jobs, which differ only in the shared-library suffix.
#
# The result is zipped wholesale by the release workflow, so what lands here is
# exactly what ships: nothing filters it downstream.
#
#     package-artifact.sh BUILD_DIR OUT_DIR
#
# BUILD_DIR is a libquil tree that has been built; OUT_DIR receives a libquil/
# subdirectory, the prefix consumers unzip into place.

set -euo pipefail

BUILD_DIR="${1:?usage: package-artifact.sh BUILD_DIR OUT_DIR}"
OUT_DIR="${2:?usage: package-artifact.sh BUILD_DIR OUT_DIR}"

if [[ "$(uname)" == "Darwin" ]]
then
  SHARED_SUFFIX="dylib"
else
  SHARED_SUFFIX="so"
fi

DEST="${OUT_DIR}/libquil"
mkdir -p "${DEST}"

cp "${BUILD_DIR}/libquil.h" "${BUILD_DIR}/libquil.${SHARED_SUFFIX}" "${DEST}"

# runtime/ holds libsbcl_librarian, the core it loads, libsbcl itself and the
# runtime headers, which have to ship together -- libquil.core is located
# relative to the runtime's own path.
#
# Everything there ships except the generated C: sbcl_librarian.c is an
# intermediate that produced libsbcl_librarian and has no use to a consumer.
find "${BUILD_DIR}/runtime" -maxdepth 1 -type f ! -name '*.c' -exec cp {} "${DEST}" \;

echo "Packaged into ${DEST}:"
ls -1 "${DEST}" | sed 's/^/    /'
