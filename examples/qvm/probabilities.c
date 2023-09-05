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

  quil_program program;

  if (quilc_parse_quil("H 0; CNOT 0 1", &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  int n_qubits = 2;
  int n_probabilities = n_qubits*n_qubits;
  double wavefunction[n_probabilities];
  memset(wavefunction, 0, n_probabilities*sizeof(double));

  if (qvm_probabilities(program, wavefunction) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_probabilities");
    exit(1);
  }

  for (int i = 0; i < n_probabilities; i++) {
    printf("|%d> = %f\n", i, wavefunction[i]);
  }

  lisp_release_handle(program);

  return 0;
}
