(in-package :libquil)

(sbcl-librarian:define-handle-type compilation-metadata "quilc_compilation_metadata")

(defun compilation-metadata-len (metadata)
  (hash-table-count metadata))

(defun compilation-metadata-get-final-rewiring (metadata final-rewiring-ptr final-rewiring-len-ptr)
  (let ((rewiring (gethash "final_rewiring" metadata)))
    (setf (cffi:mem-ref (sb-alien:alien-sap final-rewiring-ptr) :pointer)
          (cffi:foreign-alloc :int :initial-contents rewiring))
    (setf (cffi:mem-ref (sb-alien:alien-sap final-rewiring-len-ptr) :int)
          (length rewiring))))

(defun compilation-metadata-get-gate-depth (metadata gate-depth-ptr)
  (multiple-value-bind (gate-depth present-p)
      (gethash "gate_depth" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap gate-depth-ptr) :int)
            gate-depth))
    present-p))

(defun compilation-metadata-get-gate-volume (metadata gate-volume-ptr)
  (multiple-value-bind (gate-volume present-p)
      (gethash "gate_volume" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap gate-volume-ptr) :int)
            gate-volume))
    present-p))

(defun compilation-metadata-get-multiqubit-gate-depth (metadata multiqubit-gate-depth-ptr)
  (multiple-value-bind (multiqubit-gate-depth present-p)
      (gethash "multiqubit_gate_depth" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap multiqubit-gate-depth-ptr) :int)
            multiqubit-gate-depth))
    present-p))

(defun compilation-metadata-get-topological-swaps (metadata topological-swaps-ptr)
  (multiple-value-bind (topological-swaps present-p)
      (gethash "topological_swaps" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap topological-swaps-ptr) :int)
            topological-swaps))
    present-p))

(defun compilation-metadata-get-program-duration (metadata duration-ptr)
  (multiple-value-bind (duration present-p)
      (gethash "program_duration" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap duration-ptr) :double)
            (coerce duration 'double-float)))
    present-p))

(defun compilation-metadata-get-program-fidelity (metadata fidelity-ptr)
  (multiple-value-bind (fidelity present-p)
      (gethash "program_fidelity" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap fidelity-ptr) :double)
            (coerce fidelity 'double-float)))
    present-p))

(defun compilation-metadata-get-qpu-runtime-estimation (metadata runtime-ptr)
  (multiple-value-bind (runtime present-p)
      (gethash "qpu_runtime_estimation" metadata)
    (when present-p
      (setf (cffi:mem-aref (sb-alien:alien-sap runtime-ptr) :double)
            (coerce runtime 'double-float)))
    present-p))

(defun compile-protoquil (parsed-program chip-specification metadata-ptr)
  (multiple-value-bind (compiled-program metadata)
      (process-program parsed-program chip-specification :protoquil t)
    (unless (null-pointer-p metadata-ptr)
      (let ((handle (sbcl-librarian::make-handle metadata)))
        (setf (sb-alien:deref metadata-ptr)
              handle)))
    (cl-quil.frontend::transform 'cl-quil.frontend::process-protoquil compiled-program)
    compiled-program))
