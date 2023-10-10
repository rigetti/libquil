#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libquil.h"
#include "error.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main() {
  init("../../libquil.core");

  quilc_version_info version_info;

  if (quilc_get_version_info(&version_info) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_get_version_info");
    exit(1);
  }

  char* version;
  char* githash;
  if (quilc_version_info_version(version_info, &version) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_version_info_version");
    exit(1);
  }
  if (quilc_version_info_githash(version_info, &githash) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_version_info_githash");
    exit(1);
  }

  printf("quilc %s (%s)\n", version, githash);

  free(version);
  free(githash);

  return 0;
}
