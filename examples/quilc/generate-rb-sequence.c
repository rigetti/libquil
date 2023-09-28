#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "libquil.h"

int main() {
  init("../../libquil.core");

  quil_program phase, h, y;
  int depth = 3, seed = 42, qubits = 1;

  if (quilc_parse_quil("PHASE(pi/2) 0", &phase) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }
  if (quilc_parse_quil("H 0", &h) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }
  if (quilc_parse_quil("Y 0", &y) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  quil_program gateset[] = {phase, h, y};

  int *results, result_lens[depth];

  if (quilc_generate_rb_sequence(depth, qubits, gateset, 3, seed, NULL,
                                 &results,
                                 result_lens) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to generate RB sequence");
    exit(1);
  }

  int offset = 0;
  for (int i = 0; i < depth; i++) {
    printf("sequence %d = [", i);
    for (int j = 0; j < result_lens[i]; j++) {
      printf("%d", results[offset + j]);
      if (j != result_lens[i] - 1) {
        printf(", ");
      } else {
        printf("]");
      }
    }
    offset += result_lens[i];
    printf("\n");
  }

  free(results);

  return 0;
}
