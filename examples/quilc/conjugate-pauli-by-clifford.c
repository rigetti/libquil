#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "libquil.h"

int main(int argc, char *argv[]) {
  init("../../libquil.core");

  quil_program clifford;
  if (quilc_parse_quil("H 0", &clifford) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  int pauli_indices[] = {0};
  char *pauli_terms[] = {"X"};
  int result_phase;
  char *result_pauli;

  if (quilc_conjugate_pauli_by_clifford(
          pauli_indices, 1, pauli_terms, 1, clifford, &result_phase,
          &result_pauli) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_conjugate_by_pauli");
    exit(1);
  }

  printf("Global phase is %d (should be 0)\n", result_phase);
  printf("Pauli term is %s (shold be Z)\n", result_pauli);

  return 0;
}
