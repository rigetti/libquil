#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "error.h"
#include "libquil.h"

void die(char *msg) {
  printf("%s\n", msg);
  exit(1);
}

double do_expectation(quil_program state_prep, quil_program operator) {
  quil_program operators[1] = {operator};
  double expectations[1] = {0};

  if (qvm_expectation(state_prep, operators, 1, NULL, &expectations) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_expectation");
    exit(1);
  }

  return expectations[0];
}

int main(int argc, char **argv) {
  init("../../libquil.core");

  quil_program i;
  quil_program z;
  quil_program x;

  if (quilc_parse_quil("I 0", &i) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }
  if (quilc_parse_quil("Z 0", &z) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }
  if (quilc_parse_quil("X 0", &x) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  double ziz_expectation = do_expectation(i, z);
  printf("<Z|I|Z> = %f (should be 1.0)\n", ziz_expectation);
  double xix_expectation = do_expectation(x, z);
  printf("<Z|X|Z> = %f (should be -1.0)\n", xix_expectation);

  lisp_release_handle(i);
  lisp_release_handle(z);
  lisp_release_handle(x);

  return 0;
}
