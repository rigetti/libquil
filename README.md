# libquil

`libquil` provides a C API for [Quilc](https://github.com/quil-lang/quilc) and [QVM](https://github.com/quil-lang/qvm).

# C API Reference

## Libquil functions and types

- `libquil_error_t`
  Enum which indicates whether a function call was successful (`LIBQUIL_ERROR_SUCCESS`) or not (`LIBQUIL_ERROR_FAIL`). Most functions will have this as their return type.
- `libquil_error_t libquil_error(char** error_msg)`
  Used to retrieve the last error message from libquil.

  When any error is encountered by libquil, it will be stored in memory. A subsequent call to `libquil_error` will return that error message. After calling `libquil_error`, the error is cleared from memory such that immediately calling `libquil_error` after a previous call will return an empty string (indicating no errors since the previous error).

## Quilc functions and types

- `quil_program`
  An opaque pointer to a Quilc program object
- `chip_specification`
  An opaque pointer to a Quilc chip specification object
- `quilc_version_info`
  An opaque pointer to a Quilc version object

  See [examples/quilc/version.c](examples/quilc/version.c)

- `libquil_error quilc_get_version_info(quilc_version_info *version_info)`
  Allocates a `quilc_version_info` object and stores the pointer to it in `version_info`

  See [examples/quilc/version.c](examples/quilc/version.c)

- `libquil_error quilc_version_info_version(quilc_version_info version_info, char** version)`
  Allocates memory which indicates the version string of Quilc and stores the pointer to it in `version`

  See [examples/quilc/version.c](examples/quilc/version.c)

- `libquil_error quilc_version_info_githash(quilc_version_info version_info, char** githash)`
  Allocates memory which indicates the githash string of Quilc and stores the pointer to it in `version`

  See [examples/quilc/version.c](examples/quilc/version.c)

- `libquil_error_t quilc_parse_quil(char* program, quil_program *result)`
  Parses the `program` string and stores it in a `quil_program`
- `libquil_error_t quilc_print_program(quil_program program)`
  Prints the `program` to stdout
- `libquil_error_t quilc_program_string(quil_program program, char** result)`
  Allocates and populates a `char*` which is the given `program`'s string representation
- `libquil_error_t quilc_compile_quil(quil_program program, chip_specification chip_spec, quil_program* compiled_program)`
  Compiles the `program` for the provided chip specification and stores it in a `quil_program`
- `libquil_error_t quilc_compile_protoquil(quil_program program, chip_specification chip_spec, quil_program* compiled_program)`
  Compiles the (protoquil) `program` for the provided chip specification and stores it in a `quil_program`
- `libquil_error_t quilc_conjugate_pauli_by_clifford(void* pauli_indices, int pauli_indices_len, void* pauli_terms, int pauli_terms_len, quil_program clifford, void* phase, void* pauli)`
  Conjugates a Pauli operator by a Clifford operator

  After having called `quilc_conjugate_pauli_by_clifford`:

  - `phase` will be the encoded global phase factor
  - `pauli` will be a string description of the resulting encoded Pauli operator

- `libquil_error_t quilc_generate_rb_sequence(int depth, int qubits, void* gateset_ptr, int gateset_len, int seed, void* interleaver, void* results_ptr, void* result_lens_ptr)`
  Generates a randomized benchmarking sequence

  After having called `quilc_generate_rb_sequence`:

  - `result_lens_ptr` (a pre-allocated `int` array of length `depth`) will contain integers describing the segmentation of `results_ptr`
  - `results_ptr` will be populated with a flat array of integer values, the interpretation of which depends on `results_len_ptr`. This memory should be freed with `free`.

  See [examples/quilc/generate-rb-sequence.c](examples/quilc/generate-rb-sequence.c)

- `libquil_error_t quilc_build_nq_linear_chip(int n, chip_specification* chip_spec)`
  Builds a linearly-connected `n`-qubit chip specification and stores it in `chip_spec`
- `libquil_error_t quilc_chip_spec_from_isa_descriptor(char* isa_json, chip_specification* chip_spec)`
  Builds an arbitrary chip specification using the JSON-encoded ISA description

## QVM functions and types

- `qvm_multishot_addresses`
  An opaque pointer to a QVM multishot addresses object
- `libquil_error_t qvm_multishot_result`
  An opaque pointer to a QVM multishot result object
- `libquil_error_t qvm_version_info`
  An opaque pointer to a QVM version info object
- `libquil_error_t qvm_get_version_info(qvm_version_info* version_info)`
  Get a new `qvm_version_info`
- `libquil_error_t qvm_version_info_version(qvm_version_info version_info, char** version)`
  Populate a string at `*version` which has the QVM version
- `libquil_error_t qvm_version_info_githash(qvm_version_info version_info, char** version)`
  Populate a string at `*githash` which has the QVM githash
- `libquil_error_t qvm_multishot_addresses_new(qvm_multishot_addresses* addresses)`
  Allocate memory for the `qvm_multishot_addresses` object
- `libquil_error_t qvm_multishot_addresses_set(qvm_multishot_addresses addresses, char* name, void* indices, int len)`
  Set the indices of a memory region which should be collected when using `qvm_multishot`.

  For example, if your register was named `ro` and you wanted to get indices 0 and 2, you would provide `"ro"` for `name` and `{0, 2}` for `indices`. (`len` is the length of `indices`.)

- `libquil_error_t qvm_multishot(quil_program program, qvm_multishot_addresses addresses, int trials, qvm_multishot_result *result)`
  Execute `program` on the QVM `trials`-number of times, collecting the `addresses` into `result`

  See [examples/qvm/multishot.c](examples/qvm/multishot.c)

- `libquil_error_t qvm_multishot_result_get(qvm_multishot_result qvm_result, char* region_name, int region_index, void* result)`
  Get the measurement results for `region_index` in `region_name`, storing the data in the pre-allocated `result`

  `result` should be a pointer to memory which has been allocated to store `N` integers, where `N` is the `trials` value used for `qvm_multishot`.

  See [examples/qvm/multishot.c](examples/qvm/multishot.c)

- `libquil_error_t qvm_multishot_measure(quil_program program, void* qubits, int n_qubits, int trials, void* result)`
  Execute `program` on the QVM `trials`-number of times, storing measurement results for the specified `qubits` into `result`

  `result` should be a pointer to memory which has been allocated to store `n_qubits * trials` integers. This memory should be interpreted as the two-dimensional array whose outer dimension is `trials` and whose inner dimension is `n_qubits`.

  See [examples/qvm/multishot-measure.c](examples/qvm/multishot-measure.c)

- `libquil_error_t qvm_expectation(quil_program state_prep, void* operators, int n_operators, void* result)`
  Calculate the expectation value `<O|P|O>` where `P` is the provided state preparation program `state_prep` and `O` is an operator, for each `O` in `operators`.

  `result` should be a pointer to memory that has been allocated to store `n_operators` double-precision floats.

  See [examples/qvm/expectation.c](examples/qvm/expectation.c)

- `libquil_error_t qvm_wavefunction(quil_program program, void* result)`
  Execute `program` on the QVM and return the associated wavefunction

  `result` should be a pointer to memory that has been allocated to store `2 * N_q^2` double-precision floats where `N_q` is the number of qubits used in the program. The factor of 2 is here to account for the fact that the wavefunction is a complex-valued vector, thus to represent it in C we need two values: one for the real part and one for the imaginary part. `result` can be interpreted as a two-dimensional array whose outer dimension (size `N_q^2`) is the wavefunction vector index, and whose inner dimension (size `2`) is the pair of values making up the complex value.

  See [examples/qvm/wavefunction.c](examples/qvm/wavefunction.c)

- `libquil_error_t qvm_probabilities(quil_program program, void* results_ptr)`
  Execute `program` on the QVM and return the wavefunction probabilities

  `result` should be a pointer to memory that has been allocated to store `N_q^2` double-precision floats where `N_q` is the number of qubits used in the program. Each index `i` in `result` is the probability of finding the wavefunction in the `|i>` state.

  See [examples/qvm/probabilities.c](examples/qvm/probabilities.c)

## Lisp functions and types

- `void lisp_release_handle(void* handle)`
  Frees the Lisp-allocated memory for `handle`
- `int lisp_handle_eq(void* a, void* b)`
  Compare Lisp object handles
- `int init(char* core)`
  Initialize the Lisp runtime using the provided `core`

  This **MUST** be performed before using any of the above functions.
