#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "libquil.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  init("../../libquil.core");

  quil_program program;

  char *source =
      "DECLARE ro BIT[3]; H 0; CNOT 0 1; MEASURE 0 ro[0]; MEASURE 1 ro[1]";

  if (quilc_parse_quil(source, &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  qvm_multishot_addresses addresses;
  if (qvm_multishot_addresses_new(&addresses) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to create addresses");
    exit(1);
  }

  int indices[2] = {0, 1};
  if (qvm_multshot_adddresses_set(addresses, "ro", indices, 2) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to set address indices");
    exit(1);
  }

  qvm_multishot_result qvm_res;
  int num_trials = 10;
  if (qvm_multishot(program, addresses, num_trials, &qvm_res) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_multishot");
    exit(1);
  }

  for (int i = 0; i < num_trials; i++) {
    int vals[2];
    if (qvm_multishot_result_get(qvm_res, "ro", i, &vals) !=
        LIBQUIL_ERROR_SUCCESS) {
      LIBQUIL_ERROR("failed to call qvm_multishot_result_get");
      exit(1);
    }
    printf("Trial %d\n\tro[0]=%d\n\tro[1]=%d\n", i, vals[0], vals[1]);
  }

  lisp_release_handle(qvm_res);
  lisp_release_handle(program);

  return 0;
}
