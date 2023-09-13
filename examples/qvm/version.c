#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libquil.h"
#include "error.h"

int main(int argc, char **argv) {
  init("../../libquil.core");

  char* version;

  if (qvm_get_version_info(&version) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_get_version_info");
    exit(1);
  }

  printf("QVM %s\n", version);

  return 0;
}
