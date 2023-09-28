(in-package :libquil)

(sbcl-librarian:define-handle-type quilc-version-info "quilc_version_info")
(sbcl-librarian:define-handle-type quil-program "quil_program")
(sbcl-librarian:define-handle-type chip-specification "chip_specification")

(defun quilc-get-version-info ()
  (let ((version quilc::+QUILC-VERSION+)
        (githash quilc::+GIT-HASH+)
        (version-info (make-hash-table :test #'equal)))
    (setf (gethash "version" version-info) version)
    (setf (gethash "githash" version-info) githash)
    version-info))

(defun quilc-version-info-version (version-info)
  (gethash "version" version-info))

(defun quilc-version-info-githash (version-info)
  (gethash "githash" version-info))

(defun compile-protoquil (parsed-program chip-specification)
  (let ((compiled-program (cl-quil::compiler-hook parsed-program chip-specification :protoquil t)))
    (cl-quil.frontend::transform 'cl-quil.frontend::process-protoquil compiled-program)
    compiled-program))

(defun program-to-string (program)
  (with-output-to-string (s)
    (cl-quil.frontend:print-parsed-program program s)))

(defun parse-chip-spec-isa-json (isa-json)
  (time (cl-quil::qpu-hash-table-to-chip-specification (yason:parse isa-json))))

(defun conjugate-pauli-by-clifford (pauli-indices-ptr pauli-indices-len pauli-terms-ptr pauli-terms-len clifford-program phase-ptr pauli-ptr)
  (let ((pauli-indices (unpack-c-array-to-lisp-list pauli-indices-ptr pauli-indices-len :int))
        (pauli-terms (unpack-c-array-to-lisp-list pauli-terms-ptr pauli-terms-len :string)))
    (let* ((clifford-indices (sort (cl-quil:qubits-used clifford-program) #'<))
           (qubits (sort (union (copy-seq pauli-indices) (copy-seq clifford-indices)) #'<)))
      (let* ((pauli (cl-quil.clifford:pauli-from-string
                     (reverse (with-output-to-string (s)
                                (dolist (i qubits)
                                  (cond ((member i pauli-indices)
                                         (write-string (nth (position i pauli-indices) pauli-terms) s))
                                        (t
                                         (write-string "I" s))))))))
             (clifford (cl-quil.clifford:embed
                        (cl-quil.clifford::clifford-circuit-p clifford-program)
                        (length qubits)
                        (reverse (loop :for index :in clifford-indices :collect (position index qubits)))))
             (result (cl-quil.clifford::apply-clifford clifford pauli)))
        (setf (cffi:mem-aref (sb-alien:alien-sap phase-ptr) :int)
              (cl-quil.clifford::phase-factor result))
        (setf (cffi:mem-aref (sb-alien:alien-sap pauli-ptr) :string)
              (apply #'concatenate 'string
                     (mapcar (alexandria:compose #'symbol-name #'cl-quil.clifford::base4-to-sym)
                             (cl-quil.clifford::base4-list result))))))))

(defun generate-rb-sequence (depth n-qubits gateset-ptr gateset-len seed-ptr interleaver-ptr results-ptr result-lens-ptr)
  (let ((gateset (unpack-c-array-to-list-of-quil-program gateset-ptr gateset-len))
        (seed (unpack-maybe-nil-pointer seed-ptr :int))
        (interleaver (unpack-maybe-nil-pointer-to-quil-program interleaver-ptr)))
    (let* ((cliffords (mapcar #'cl-quil.clifford::clifford-circuit-p gateset))
           (qubits-used (mapcar #'cl-quil:qubits-used gateset))
           (qubits-used-by-interleaver
             (when interleaver
               (cl-quil:qubits-used interleaver)))
           (qubits (union qubits-used-by-interleaver (reduce #'union qubits-used)))
           (embedded-cliffords
             (loop :for clifford :in cliffords
                   :for i :from 0
                   :collect (cl-quil.clifford:embed
                             clifford n-qubits
                             (reverse (loop :for index :in (nth i qubits-used)
                                            :collect (position index qubits))))))
           (embedded-interleaver
             (when interleaver
               (cl-quil.clifford:embed (cl-quil.clifford::clifford-circuit-p interleaver)
                                       n-qubits
                                       ;; XXX: the embedding ordering has
                                       ;; been reversed to comply with
                                       ;; the computational basis
                                       ;; convention, hence the reverse
                                       ;; here. We could use a better fix
                                       ;; for this.
                                       (reverse (loop :for index :in qubits-used-by-interleaver
                                                      :collect (position index qubits)))))))
      (let* ((rb-sequence
               (let ((*random-state*
                       (if seed (sb-ext:seed-random-state seed) *random-state*)))
                 (cl-quil.clifford::rb-sequence depth n-qubits embedded-cliffords embedded-interleaver)))
             (gateset-label-sequence
               (loop :for clifford-element :in rb-sequence
                     :collect (loop :for generator :in clifford-element
                                    :collect (position generator embedded-cliffords :test #'cl-quil.clifford:clifford=))))
             (flattened-gls (alexandria:flatten gateset-label-sequence)))
        (let ((ptr (cffi:foreign-alloc :int :initial-contents flattened-gls)))
          (setf (cffi:mem-ref (sb-alien:alien-sap results-ptr) :pointer) ptr))
        (loop :for gls :in gateset-label-sequence
              :for i :from 0
              :do
                 (setf (cffi:mem-aref (sb-alien:alien-sap result-lens-ptr) :int i)
                       (length gls)))))))

(sbcl-librarian:define-api quilc (:error-map error-map
                                  :function-prefix "quilc_")
  (:literal "/* Quilc types */")
  (:type quil-program chip-specification quilc-version-info)
  (:literal "/* Quilc functions */")
  (:function
   (("get_version_info" quilc-get-version-info) quilc-version-info ())
   (("version_info_version" quilc-version-info-version) :string ((version-info quilc-version-info)))
   (("version_info_githash" quilc-version-info-githash) :string ((version-info quilc-version-info)))
   (("parse_quil" cl-quil.frontend:safely-parse-quil) quil-program ((source :string)))
   (("print_program" cl-quil.frontend:print-parsed-program) :void ((program quil-program)))
   (("compile_quil" cl-quil:compiler-hook) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("compile_protoquil" compile-protoquil) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("build_nq_linear_chip" cl-quil::build-nq-linear-chip) chip-specification ((n :int)))
   (("chip_spec_from_isa_descriptor" quilc::lookup-isa-descriptor-for-name) chip-specification ((descriptor :string)))
   (("print_chip_spec" cl-quil::debug-print-chip-spec) :void ((chip-spec chip-specification)))
   (("parse_chip_spec_isa_json" parse-chip-spec-isa-json) chip-specification ((isa-json :string)))
   (("program_string" program-to-string) :string ((program quil-program)))
   (("conjugate_pauli_by_clifford" conjugate-pauli-by-clifford)
    :void
    ((pauli-indices :pointer)
     (pauli-indices-len :int)
     (pauli-terms :pointer)
     (pauli-terms-len :int)
     (clifford quil-program)
     (phase :pointer)
     (pauli :pointer)))
   (("generate_rb_sequence" generate-rb-sequence)
    :void
    ((depth :int)
     (qubits :int)
     (gateset-ptr :pointer)
     (gateset-len :int)
     (seed-ptr :pointer)
     (interleaver :pointer)
     (results-ptr :pointer)
     (result-lens-ptr :pointer)))))
