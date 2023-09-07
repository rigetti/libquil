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

  char* source = "X 0; X 2";

  if (quilc_parse_quil(source, &program) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call quilc_parse_quil");
    exit(1);
  }

  int num_trials = 10;
  int n_qubits = 2;
  int qubits[n_qubits];
  memset(qubits, 0, n_qubits*sizeof(int));
  int results[num_trials][n_qubits];
  memset(results, 0, num_trials * n_qubits * sizeof(int));
  if (qvm_multishot_measure(program, qubits, n_qubits, num_trials, &results) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to call qvm_multishot_measure");
    exit(1);
  }

  for (int trial = 0; trial < num_trials; trial++) {
    printf("Trial %d\n", trial);
    for (int qubit = 0; qubit < n_qubits; qubit++) {
      printf("\tro[%d] = %d\n", qubit, results[trial][qubit]);
    }
    printf("\n");
  }

  lisp_release_handle(program);

  return 0;
}
