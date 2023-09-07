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

  if (quilc_parse_quil("X 0; I 1", &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  int n_qubits = 2;
  int n_amplitudes = n_qubits*n_qubits;
  double wavefunction[n_amplitudes][2];
  memset(wavefunction, 0, n_amplitudes*2*sizeof(double));

  if (qvm_wavefunction(program, wavefunction) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_wavefunction");
    exit(1);
  }

  for (int i = 0; i < n_amplitudes; i++) {
    printf("|%d> = %f + i%f\n", i, wavefunction[i][0], wavefunction[i][1]);
  }

  lisp_release_handle(program);

  return 0;
}
