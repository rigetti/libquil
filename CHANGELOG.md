## 0.4.0-rc.1 (2026-08-26)

### Breaking Changes

- build against modern sbcl-librarian

### Features

- install to any prefix, and need root only when the prefix demands it
- add `make install`, and document installing without root

### Fixes

- load OpenBLAS into the image before magicl picks a backend
- check for BLAS and LAPACK, and install without sudo when already root
- say what the SUDO_USER guard in install.sh actually checks

## 0.4.0-rc.0 (2026-08-13)

### Breaking Changes

- build against modern sbcl-librarian

### Fixes

- load OpenBLAS into the image before magicl picks a backend
- check for BLAS and LAPACK, and install without sudo when already root

## 0.3.2 (2023-12-14)

### Fixes

#### compress qubits in multishot (#55)

## 0.3.1 (2023-12-11)

### Fixes

#### mask float traps (#53)

## 0.3.0 (2023-11-16)

### Breaking Changes

#### adds a `rng_seed` parameter to QVM functions

### Fixes

#### Provide RNG seed for QVM (#47)

## 0.2.0 (2023-11-16)

### Breaking Changes

#### Adds two parameters for gate and measurement noise to `multishot`

### Fixes

#### include gate and measurement noise (#45)

## 0.1.1 (2023-11-15)

### Features

#### Support all Quil types in multishot results (#43)

## 0.1.0 (2023-10-27)

### Breaking Changes

#### memory passed to wavefunction should be a double**

### Features

#### foreign allocate wavefunction memory (#40)

## 0.0.6 (2023-10-27)

### Features

#### add quilc_multishot_(set|get)_all (#38)

## 0.0.5 (2023-10-24)

### Features

#### remove #:quilc and #:qvm-app dependencies (#31)

## 0.0.4 (2023-10-18)

### Features

#### Build macos release artifacts (#26)

## 0.0.3 (2023-10-11)

### Features

#### support conjugate_pauli_by_clifford and generate_rb_sequence

#### provide full access to the compilation metadata hash-map

### Fixes

#### don't parse interleaver

#### allow for optional seed

#### foreign alloc return values

#### remove time debug

## 0.0.2 (2023-09-22)

### Fixes

#### test ci
