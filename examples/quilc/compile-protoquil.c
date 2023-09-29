#include <stdio.h>
#include <stdlib.h>

#include "error.h"
#include "libquil.h"

int main() {
  init("../../libquil.core");

  quil_program h, result;
  chip_specification chip;

  if (quilc_parse_quil("CNOT 0 1", &h) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to parse quil");
    exit(1);
  }

  if (quilc_build_nq_linear_chip(2, &chip) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to build chip");
    exit(1);
  }

  quilc_compilation_metadata metadata;

  if (quilc_compile_protoquil(h, chip, &metadata, &result) !=
      LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to compile program");
    exit(1);
  }

  int *final_rewiring, final_rewiring_len;
  if (quilc_compilation_metadata_get_final_rewiring(metadata, &final_rewiring, &final_rewiring_len) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to final rewiring from metadata");
    exit(1);
  }

  for (int i = 0; i < final_rewiring_len; i++) {
    printf("%d rewiring to %d\n", i, final_rewiring[i]);
  }

  int gate_depth, multiqubit_gate_depth, gate_volume, topological_swaps, present;
  double program_duration, program_fidelity, qpu_runtime_estimation;

  if (quilc_compilation_metadata_get_gate_depth(metadata, &gate_depth, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get gate depth");
    exit(1);
  }
  if (present) {
    printf("gate depth is %d\n", gate_depth);
  }

  if (quilc_compilation_metadata_get_multiqubit_gate_depth(metadata, &multiqubit_gate_depth, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get multiqubit gate depth");
    exit(1);
  }
  if (present) {
    printf("multiqubit gate depth is %d\n", gate_depth);
  }

  if (quilc_compilation_metadata_get_gate_volume(metadata, &gate_volume, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get gate volume");
    exit(1);
  }
  if (present) {
    printf("gate volume is %d\n", gate_volume);
  }

  if (quilc_compilation_metadata_get_topological_swaps(metadata, &topological_swaps, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get topological swaps");
    exit(1);
  }
  if (present) {
    printf("topological swaps is %d\n", topological_swaps);
  }

  if (quilc_compilation_metadata_get_program_duration(metadata, &program_duration, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get program duration");
    exit(1);
  }
  if (present) {
    printf("program duration is %f\n", program_duration);
  }

  if (quilc_compilation_metadata_get_program_fidelity(metadata, &program_fidelity, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get program fidelity ");
    exit(1);
  }
  if (present) {
    printf("program fidelity is %f\n", program_fidelity);
  }
  
  if (quilc_compilation_metadata_get_qpu_runtime_estimation(metadata, &qpu_runtime_estimation, &present) != LIBQUIL_ERROR_SUCCESS) {
    LIBQUIL_ERROR("failed to get qpu runtime estimation ");
    exit(1);
  }
  if (present) {
    printf("qpu runtime estimation is %f\n", qpu_runtime_estimation);
  }

  quilc_print_program(result);
}
