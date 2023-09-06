#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "libquil.h"
#include "error.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  init("../../libquil.core");

  char version[32] = {'\0'};
  char githash[32] = {'\0'}; 

  printf("quilc %s (%s)\n", version, githash);

  if (quilc_get_version_info(version, githash) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_get_version_info");
    exit(1);
  }

  printf("quilc %s (%s)\n", version, githash);

  return 0;
}
