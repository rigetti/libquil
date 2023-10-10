#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libquil.h"
#include "error.h"

int main(int argc, char **argv) {
  init("../../libquil.core");

  qvm_version_info version_info;

  if (qvm_get_version_info(&version_info) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_get_version_info");
    exit(1);
  }

  char* version;
  char* githash;
  if (qvm_version_info_version(version_info, &version) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_version_info_version");
    exit(1);
  }
  if (qvm_version_info_githash(version_info, &githash) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_version_info_githash");
    exit(1);
  }

  printf("QVM %s (%s)\n", version, githash);

  free(version);
  free(githash);

  return 0;
}
