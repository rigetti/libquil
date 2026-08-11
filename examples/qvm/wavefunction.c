#include <math.h>
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

  if (quilc_parse_quil("X 0; I 1", &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  int wavefunction_len;
  double *wavefunction;
  int seed = 0;
  if (qvm_wavefunction(program, &seed, &wavefunction, &wavefunction_len) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_wavefunction");
    exit(1);
  }

  int n_amplitudes = 4;
  for (int i = 0; i < n_amplitudes; i++) {
    printf("|%d> = %f + i%f\n", i, wavefunction[i * 2],
           wavefunction[i * 2 + 1]);
  }

  lisp_release_handle(program);

  return 0;
}
