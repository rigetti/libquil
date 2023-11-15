#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "libquil.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

void multishot_with_explicit_ro_indices() {
  quil_program program;

  char *source = "DECLARE ro BIT[3]; X 0; I 1; X 2; MEASURE 0 ro[0]; MEASURE 1 "
                 "ro[1]; MEASURE 2 ro[2]";

  if (quilc_parse_quil(source, &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  qvm_multishot_addresses addresses;
  if (qvm_multishot_addresses_new(&addresses) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to create addresses");
    exit(1);
  }

  int indices[3] = {0, 1, 2};
  if (qvm_multishot_addresses_set(addresses, "ro", indices, 3) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to set address indices");
    exit(1);
  }

  qvm_multishot_result qvm_res;
  int num_trials = 10;
  if (qvm_multishot(program, addresses, num_trials, NULL, NULL, &qvm_res) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_multishot");
    exit(1);
  }

  for (int i = 0; i < num_trials; i++) {
    char vals[3];
    if (qvm_multishot_result_get(qvm_res, "ro", i, &vals) !=
        LIBQUIL_ERROR_SUCCESS) {
      LIBQUIL_ERROR("failed to call qvm_multishot_result_get");
      exit(1);
    }
    printf("Trial %d\n\tro[0]=%d\n\tro[1]=%d\n\tro[2]=%d\n", i, vals[0],
           vals[1], vals[2]);
  }

  lisp_release_handle(qvm_res);
  lisp_release_handle(program);
}

void multishot_with_implicit_ro_indices() {
  quil_program program;

  char *source = "DECLARE ro BIT[3]; X 0; I 1; X 2; MEASURE 0 ro[0]; MEASURE 1 "
                 "ro[1]; MEASURE 2 ro[2]";

  if (quilc_parse_quil(source, &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  qvm_multishot_addresses addresses;
  if (qvm_multishot_addresses_new(&addresses) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to create addresses");
    exit(1);
  }

  if (qvm_multishot_addresses_set_all(addresses, "ro") !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to set address indices");
    exit(1);
  }

  qvm_multishot_result qvm_res;
  int num_trials = 10;
  double gate_noise[] = {0.0, 0.0, 0.0};
  if (qvm_multishot(program, addresses, num_trials, &qvm_res, NULL, NULL) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_multishot");
    exit(1);
  }

  for (int i = 0; i < num_trials; i++) {
    int len;
    char *vals;

    if (qvm_multishot_result_get_all(qvm_res, "ro", i, &vals, &len) !=
        LIBQUIL_ERROR_SUCCESS) {
      LIBQUIL_ERROR("failed to call qvm_multishot_result_get_all");
      exit(1);
    }

    printf("Trial %d:\n", i);
    for (int j = 0; j < len; j++) {
      printf("\tro[%d]=%d\n", j, vals[j]);
    }
  }

  lisp_release_handle(qvm_res);
  lisp_release_handle(program);
}

void multishot_with_noise() {
  quil_program program;

  char *source = "DECLARE ro BIT[3]; X 0; I 1; X 2; MEASURE 0 ro[0]; MEASURE 1 "
                 "ro[1]; MEASURE 2 ro[2]";

  if (quilc_parse_quil(source, &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  qvm_multishot_addresses addresses;
  if (qvm_multishot_addresses_new(&addresses) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to create addresses");
    exit(1);
  }

  int indices[3] = {0, 1, 2};
  if (qvm_multishot_addresses_set(addresses, "ro", indices, 3) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to set address indices");
    exit(1);
  }

  qvm_multishot_result qvm_res;
  int num_trials = 10;
  double gate_noise[] = {0.1, 0.1, 0.1};
  double measurement_noise[] = {0.1, 0.0, 0.0};
  if (qvm_multishot(program, addresses, num_trials, gate_noise,
                    measurement_noise, &qvm_res) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_multishot");
    exit(1);
  }

  for (int i = 0; i < num_trials; i++) {
    char vals[3];
    if (qvm_multishot_result_get(qvm_res, "ro", i, &vals) !=
        LIBQUIL_ERROR_SUCCESS) {
      LIBQUIL_ERROR("failed to call qvm_multishot_result_get");
      exit(1);
    }
    printf("Trial %d\n\tro[0]=%d\n\tro[1]=%d\n\tro[2]=%d\n", i, vals[0],
           vals[1], vals[2]);
  }

  lisp_release_handle(qvm_res);
  lisp_release_handle(program);
}

int main(int argc, char **argv) {
  init("../../libquil.core");

  multishot_with_explicit_ro_indices();
  // multishot_with_implicit_ro_indices();

  return 0;
}
