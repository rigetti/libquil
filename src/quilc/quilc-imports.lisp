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
;;;
;;; PROCESS-PROTOQUIL and STRIP-FINAL-HALT-RESPECTING-REWIRINGS live in quilc's
;;; *application*, which we do not depend on, so they are vendored here rather
;;; than requiring a quilc fork that exposes them from the library.
;;; TODO(https://github.com/quil-lang/quilc/pull/933): remove these once the logic is upstreamed

(cl-quil.frontend::define-transform process-protoquil (process-protoquil)
  "Removes HALT, DEFCIRCUIT, and DEFGATE instructions.")

(defun process-protoquil (parsed-program)
  (setf (cl-quil.frontend::parsed-program-circuit-definitions parsed-program) nil
        (cl-quil.frontend::parsed-program-gate-definitions parsed-program) nil)

  ;; if we're supposed to output protoQuil, we also need to
  ;; strip the final HALT instructions from the output
  (setf (cl-quil:parsed-program-executable-code parsed-program)
        (strip-final-halt-respecting-rewirings parsed-program)))

(defun strip-final-halt-respecting-rewirings (processed-program)
  "Remove the final HALT instruction, if any, from PROCESSED-PROGRAM, retaining any attached rewiring comments."
  (let* ((instructions (cl-quil:parsed-program-executable-code processed-program))
         (last-instruction (and (plusp (length instructions))
                                (cl-quil::nth-instr 0 processed-program :from-end t)))
         (penultimate-instruction (and (< 1 (length instructions))
                                       (cl-quil::nth-instr 1 processed-program :from-end t)))
         (must-transfer-comment-p (and (not (null penultimate-instruction))
                                       (cl-quil.frontend::comment last-instruction))))

    (unless (cl-quil::haltp last-instruction)
      (return-from strip-final-halt-respecting-rewirings instructions))

    (when must-transfer-comment-p
      ;; Transfer the rewiring comment from LAST-INSTRUCTION to
      ;; PENULTIMATE-INSTRUCTION.
      (multiple-value-bind (last-entering last-exiting)
          (cl-quil::instruction-rewirings last-instruction)
        (multiple-value-bind (penultimate-entering penultimate-exiting)
            (cl-quil::instruction-rewirings penultimate-instruction)
          (flet ((assert-rewirings-compatible (rewiring-type last-rewiring penultimate-rewiring)
                   ;; This bit of hoop-jumping guards against the
                   ;; unlikely event that both PENULTIMATE-INSTRUCTION
                   ;; and LAST-INSTRUCTION have rewiring comments
                   ;; attached which might be incompatible. We check
                   ;; to ensure that either one of the rewirings is
                   ;; NULL, or else they are EQUALP and can safely be
                   ;; merged.
                   (assert (or (or (null last-rewiring)
                                   (null penultimate-rewiring))
                               (equalp last-rewiring penultimate-rewiring))
                       ()
                       "Failed to strip final HALT. Instructions have incompatible ~A rewirings:~@
                           LAST: ~A ~A~@
                           PREV: ~A ~A"
                       rewiring-type last-instruction last-rewiring
                       penultimate-instruction penultimate-rewiring)))
            (assert-rewirings-compatible ':ENTERING last-entering penultimate-entering)
            (assert-rewirings-compatible ':EXITING last-exiting penultimate-exiting))
          ;; Consider the following cases for the :ENTERING rewirings
          ;; (the same case analysis applies to the :EXITING rewiring
          ;; pair as well).
          ;;
          ;; 1) If both the rewirings are non-NIL, then the
          ;;    ASSERT-REWIRINGS-COMPATIBLE check above guarantees
          ;;    that they are EQUALP, and it doesn't matter which one
          ;;    we select.
          ;;
          ;; 2) If only one is non-NIL, the OR selects it.
          ;;
          ;; 3) If both are NIL, then MAKE-REWIRING-COMMENT just
          ;;    ignores that keyword argument, and returns an :EXITING
          ;;    rewiring.
          ;;
          ;; Finally, (COMMENT LAST-INSTRUCTION) is non-NIL (otherwise
          ;; MUST-TRANSFER-COMMENT-P would be NIL), so at least one of
          ;; LAST-ENTERING and LAST-EXITING is non-NIL, which means
          ;; that at least one of the :ENTERING and :EXITING keyword
          ;; args to MAKE-REWIRING-COMMENT is non-NIL and hence the
          ;; call will produce a rewiring comment.
          (setf (cl-quil.frontend::comment penultimate-instruction)
                (cl-quil::make-rewiring-comment :entering (or last-entering penultimate-entering)
                                                :exiting (or last-exiting penultimate-exiting))))))

    ;; Strip the final HALT instruction.
    (subseq instructions 0 (1- (length instructions)))))

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
        (cl-quil.frontend::transform 'process-protoquil processed-program)

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
