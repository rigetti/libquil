;;; borrowed from quilc: app/src/versions.lisp

(in-package :libquil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun system-version (system-designator)
    (let ((sys (asdf:find-system system-designator nil)))
      (if (and sys (slot-boundp sys 'asdf:version))
          (asdf:component-version sys)
          "unknown")))

  (defun git-hash (system)
    "Get the short git hash of the system SYSTEM."
    (let ((sys-path (namestring (asdf:system-source-directory system))))
      (multiple-value-bind (output err-output status)
          (uiop:run-program `("git" "-C" ,sys-path "rev-parse" "--short" "HEAD")
                            :output '(:string :stripped t)
                            :ignore-error-status t)
        (declare (ignore err-output))
        (if (not (zerop status))
            "unknown"
            output)))))

(alexandria:define-constant +QUILC-VERSION+
    (system-version '#:quilc)
  :test #'string=
  :documentation "The version of the quilc application.")

(alexandria:define-constant +QUILC-GIT-HASH+
    (git-hash '#:quilc)
  :test #'string=
  :documentation "The git hash of the quilc repo.")

;;; borrowed from quilc: app/src/entry-point.lisp

(defun process-program (program chip-specification
                        &key
                          protoquil
                          state-aware
                          enable-approximate-compilation
                          compressor-passes
                          rewriting-peephole-size
                          global-queue-tolerance-threshold
                          verbose
                          gate-whitelist
                          gate-blacklist)
  "Compile PROGRAM for the chip CHIP-SPECIFICATION. Optionally calculate statistics described by the keyword arguments. All require :PROTOQUIL T.

Note: PROGRAM is mutated by the compilation process. To avoid this, use COPY-INSTANCE.

Returns a values tuple (PROCESSED-PROGRAM, STATISTICS), where PROCESSED-PROGRAM is the compiled program, and STATISTICS is a HASH-TABLE whose keys are the slots of the RPCQ::|NativeQuilMetadata| class."
  (let* ((statistics (make-hash-table :test #'equal))
         (cl-quil::*compiler-noise* verbose)
         (*random-state* (make-random-state t))
         (cl-quil::*enable-state-prep-compression* state-aware)
         (cl-quil::*enable-approximate-compilation* enable-approximate-compilation)
         (cl-quil::*compressor-passes* (or compressor-passes cl-quil::*compressor-passes*))
         (cl-quil::*rewriting-peephole-size* (or rewriting-peephole-size cl-quil::*rewriting-peephole-size*))
         (cl-quil::*global-queue-tolerance-threshold* (or global-queue-tolerance-threshold cl-quil::*global-queue-tolerance-threshold*))
         )
    ;; do the compilation
    (multiple-value-bind (processed-program topological-swaps)
        (cl-quil:compiler-hook program chip-specification :protoquil protoquil :destructive t)

      (when protoquil
        (cl-quil.frontend::transform 'cl-quil::process-protoquil processed-program)

        ;; Compute statistics for protoquil program
        (compute-statistics processed-program chip-specification statistics :gate-whitelist gate-whitelist :gate-blacklist gate-blacklist)
        (setf (gethash "topological_swaps" statistics) topological-swaps))

      (values processed-program statistics))))

(defun compute-statistics (processed-program chip-specification statistics &key gate-whitelist gate-blacklist)
  "Compute statistics about protoquil program PROCESSED-PROGRAM.

This function will have undefined behavior when PROCESSED-PROGRAM is not protoquil."
  (setf (gethash "final_rewiring" statistics)
        (cl-quil::extract-final-exit-rewiring-vector processed-program))

  (let ((lschedule (cl-quil::make-lschedule)))
    (loop :for instr :across (cl-quil:parsed-program-executable-code processed-program)
          :unless (typep instr 'cl-quil:pragma)
            :do (cl-quil::append-instruction-to-lschedule lschedule instr))
    (setf (gethash "logical_schedule" statistics)
          lschedule))

  ;; gate depth, gate volume, duration, and fidelity stats can
  ;; all share an lschedule
  (let ((lschedule (cl-quil::make-lschedule)))
    (loop :for instr :across (cl-quil:parsed-program-executable-code processed-program)
          :when (and (typep instr 'cl-quil:gate-application)
                     (not (member (cl-quil::application-operator-root-name instr)
                                  gate-blacklist
                                  :test #'string=))
                     (or (null gate-whitelist)
                         (member (cl-quil::application-operator-root-name instr)
                                 gate-whitelist
                                 :test #'string=)))
            :do (cl-quil::append-instruction-to-lschedule lschedule instr))

    (setf (gethash "gate_depth" statistics)
          (cl-quil::lschedule-calculate-depth lschedule))

    (setf (gethash "gate_volume" statistics)
          (cl-quil::lschedule-calculate-volume lschedule))

    (setf (gethash "program_duration" statistics)
          (cl-quil::lschedule-calculate-duration lschedule chip-specification))

    (setf (gethash "program_fidelity" statistics)
          (cl-quil::lschedule-calculate-fidelity lschedule chip-specification))

    (let* ((lschedule-resources
             (let ((collect (cl-quil::make-null-resource)))
               (cl-quil::lschedule-walk-graph
                lschedule
                :bump-value (lambda (instr value)
                              (setf collect
                                    (cl-quil::resource-union collect
                                                             (cl-quil::instruction-resources instr)))
                              value))
               collect))
           (unused-qubits
             (loop :for i :below (cl-quil::chip-spec-n-qubits chip-specification)
                   :unless (cl-quil::resources-intersect-p (cl-quil::make-qubit-resource i)
                                                           lschedule-resources)
                     :collect i)))
      (setf (gethash "unused_qubits" statistics)
            unused-qubits)))

  ;; multiq gate depth requires a separate lschedule
  (let ((lschedule (cl-quil::make-lschedule)))
    (loop :for instr :across (cl-quil:parsed-program-executable-code processed-program)
          :when (and (typep instr 'cl-quil:gate-application)
                     (<= 2 (length (cl-quil:application-arguments instr))))
            :do (cl-quil::append-instruction-to-lschedule lschedule instr)
          :finally
             (setf (gethash "multiqubit_gate_depth" statistics)
                   (cl-quil::lschedule-calculate-depth lschedule))))

  statistics)
