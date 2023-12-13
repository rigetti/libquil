(in-package :libquil)

(sbcl-librarian:define-handle-type qvm-version-info "qvm_version_info")

(defun qvm-get-version-info ()
  (let ((version +QVM-VERSION+)
        (githash +QVM-GIT-HASH+)
        (version-info (make-hash-table :test #'equal)))
    (setf (gethash "version" version-info) version)
    (setf (gethash "githash" version-info) githash)
    version-info))

(defun qvm-version-info-version (version-info ptr)
  (foreign-alloc-and-set-string ptr (gethash "version" version-info)))

(defun qvm-version-info-githash (version-info ptr)
  (foreign-alloc-and-set-string ptr (gethash "githash" version-info)))

(defun qvm-multishot-addresses-new ()
  (make-hash-table :test #'equal))

(defun qvm-multishot-addresses-set (addresses region-name region-indices-ptr len)
  (let ((indices (unpack-c-array-to-lisp-list region-indices-ptr len :int)))
    (setf (gethash region-name addresses) indices)))

(defun qvm-multishot-addresses-set-all (addresses region-name)
  (setf (gethash region-name addresses) t))

(sbcl-librarian:define-handle-type qvm-multishot-result "qvm_multishot_result")

(defstruct qvm-multishot-result
  results-map
  program-memory-descriptors)

(defun qvm-multishot (compiled-quil addresses trials gate-noise-ptr measurement-noise-ptr rng-seed-ptr)
  "Executes COMPILED-QUIL on a pure-state QVM TRIALS numbers of times. At the end of each execution, the measurements for ADDRESSES are collected. The return value is a list of those measurements."
  (let* ((compressed-quil (compress-program-qubits compiled-quil))
         (num-qubits (cl-quil.frontend::qubits-needed compressed-quil))
         (gate-noise (unless (null-pointer-p gate-noise-ptr)
                       (unpack-c-array-to-lisp-list gate-noise-ptr 3 :double)))
         (measurement-noise (unless (null-pointer-p measurement-noise-ptr)
                              (unpack-c-array-to-lisp-list measurement-noise-ptr 3 :double)))
         (rng-seed (unpack-maybe-nil-pointer rng-seed-ptr :int))
         (results
           (qvm:with-random-state ((get-random-state rng-seed))
             (%perform-multishot compressed-quil num-qubits addresses trials gate-noise measurement-noise))))
    (make-qvm-multishot-result
     :results-map results
     :program-memory-descriptors (cl-quil:parsed-program-memory-definitions compressed-quil))))

(defun qvm-multishot-result-get (multishot-result address-name shot-index result-pointer)
  (let* ((results-map (qvm-multishot-result-results-map multishot-result))
         (memory-descriptors (qvm-multishot-result-program-memory-descriptors multishot-result))
         (memory-descriptor (find address-name memory-descriptors
                                  :key #'cl-quil:memory-descriptor-name
                                  :test #'equalp))
         (results (elt (gethash address-name results-map) shot-index)))
    (loop :for val :in results
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap result-pointer)
                                 (memory-descriptor-type-to-cffi-type
                                  (cl-quil:memory-descriptor-type memory-descriptor))
                                 i)
                  val))))

(defun memory-descriptor-type-to-cffi-type (descriptor-type)
  (adt:match cl-quil:quil-type descriptor-type
    (cl-quil:quil-bit :char)
    (cl-quil:quil-octet :char)
    (cl-quil:quil-integer :int)
    (cl-quil:quil-real :double)))

(defun qvm-multishot-result-get-all (multishot-result address-name shot-index result-ptr result-len-ptr)
  (let* ((results-map (qvm-multishot-result-results-map multishot-result))
         (results (elt (gethash address-name results-map) shot-index))
         (len (length results))
         (descriptors (qvm-multishot-result-program-memory-descriptors multishot-result))
         (memory-descriptor (find address-name descriptors :key #'cl-quil:memory-descriptor-name :test #'equalp))
         (cffi-type (memory-descriptor-type-to-cffi-type
                     (cl-quil:memory-descriptor-type memory-descriptor)))
         (ptr (cffi:foreign-alloc cffi-type :initial-contents results)))
    (setf (cffi:mem-ref (sb-alien:alien-sap result-ptr) :pointer) ptr)
    (setf (cffi:mem-ref (sb-alien:alien-sap result-len-ptr) :int) len)))

(defun qvm-multishot-measure (compiled-quil qubits-ptr n-qubits trials rng-seed-ptr results-ptr)
  (let ((qubits (unpack-c-array-to-lisp-list qubits-ptr n-qubits :int))
        (rng-seed (unpack-maybe-nil-pointer rng-seed-ptr :int)))
    (multiple-value-bind (compiled-quil relabeling)
        (compress-program-qubits compiled-quil)
      (let* ((num-qubits (cl-quil:qubits-needed compiled-quil))
             (results
               (qvm:with-random-state ((get-random-state rng-seed))
                 (%perform-multishot-measure
                  compiled-quil
                  num-qubits
                  qubits
                  trials
                  relabeling))))
        (loop :for trial :in results
              :for i :below trials :do
                (loop :for value :in trial
                      :for j :from 0 :do
                        (setf (cffi:mem-aref
                               (sb-alien:alien-sap results-ptr) :int (+ (* i (length trial)) j))
                              value)))))))

(defun qvm-expectation (state-prep operators-ptr n-operators rng-seed-ptr results-ptr)
  (let* ((operator-programs (unpack-c-array-to-list-of-quil-program operators-ptr n-operators))
         (num-qubits
           (loop :for p :in (cons state-prep operator-programs)
                 :maximize (cl-quil:qubits-needed p)))
         (rng-seed (unpack-maybe-nil-pointer rng-seed-ptr :int))
         (expectations
           (qvm:with-random-state ((get-random-state rng-seed))
             (%perform-expectation
              #'pure-state-expectation
              state-prep operator-programs num-qubits nil nil))))
    (loop :for expectation :in expectations
          :for i :below n-operators :do
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double i)
                  expectation))))

(defun qvm-wavefunction (program rng-seed-ptr results-ptr results-len-ptr)
  (let* ((compressed-program (compress-program-qubits program))
         (num-qubits (cl-quil:qubits-needed compressed-program))
         (rng-seed (unpack-maybe-nil-pointer rng-seed-ptr :int))
         (qvm (qvm:with-random-state ((get-random-state rng-seed))
                (%execute-quil compressed-program num-qubits nil nil)))
         (amplitudes (qvm::amplitudes qvm))
         (amplitude-pairs
           (alexandria:flatten
            (loop :for amplitude :across amplitudes
                  :collect (list (realpart amplitude) (imagpart amplitude)))))
         (ptr (cffi:foreign-alloc :double :initial-contents amplitude-pairs)))
    (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :pointer)
          ptr)
    (setf (cffi:mem-aref (sb-alien:alien-sap results-len-ptr) :int)
          (length amplitude-pairs))))

(defun qvm-probabilities (program rng-seed-ptr results-ptr)
  (let* ((compressed-program (compress-program-qubits program))
         (num-qubits (cl-quil:qubits-needed compressed-program))
         (rng-seed (unpack-maybe-nil-pointer rng-seed-ptr :int))
         (probabilities (multiple-value-bind (_ probabilities)
                            (qvm:with-random-state ((get-random-state rng-seed))
                              (%perform-probabilities compressed-program num-qubits))
                          (declare (ignore _))
                          probabilities)))
    (loop :for probability :across probabilities
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double i)
                  probability))))

(sbcl-librarian:define-api qvm (:error-map error-map :function-prefix "qvm_")
  (:literal "/* QVM types */")
  (:type qvm-multishot-addresses qvm-multishot-result qvm-version-info)
  (:literal "/* QVM functions */")
  (:function
   (("get_version_info" qvm-get-version-info)
    qvm-version-info
    ())
   (("version_info_version" qvm-version-info-version)
    :void
    ((version-info qvm-version-info)
     (result-ptr :pointer)))
   (("version_info_githash" qvm-version-info-githash)
    :void
    ((version-info qvm-version-info)
     (result-ptr :pointer)))
   (("multishot_addresses_new" qvm-multishot-addresses-new)
    qvm-multishot-addresses
    ())
   (("multishot_addresses_set" qvm-multishot-addresses-set)
    :void
    ((addresses qvm-multishot-addresses)
     (name :string)
     (indices :pointer)
     (len :int)))
   (("multishot_addresses_set_all" qvm-multishot-addresses-set-all)
    :void
    ((addresses qvm-multishot-addresses)
     (name :string)))
   (("multishot" qvm-multishot)
    qvm-multishot-result
    ((program quil-program)
     (addresses qvm-multishot-addresses)
     (trials :int)
     (gate-noise :pointer)
     (measurement-noise :pointer)
     (rng-seed :pointer)))
   (("multishot_result_get" qvm-multishot-result-get)
    :void
    ((qvm-result qvm-multishot-result)
     (region-name :string)
     (region-index :int)
     (result :pointer)))
   (("multishot_result_get_all" qvm-multishot-result-get-all)
    :void
    ((qvm-result qvm-multishot-result)
     (region-name :string)
     (shot-index :int)
     (result :pointer)
     (result-len :pointer)))
   (("multishot_measure" qvm-multishot-measure)
    :void
    ((program quil-program)
     (qubits :pointer)
     (n-qubits :int)
     (trials :int)
     (rng-seed :pointer)
     (result :pointer)))
   (("expectation" qvm-expectation)
    :void
    ((state-prep quil-program)
     (operators :pointer)
     (n-operators :int)
     (rng-seed :pointer)
     (results-ptr :pointer)))
   (("wavefunction" qvm-wavefunction)
    :void
    ((program quil-program)
     (rng-seed :pointer)
     (results-ptr :pointer)
     (results-len-ptr :pointer)))
   (("probabilities" qvm-probabilities)
    :void
    ((program quil-program)
     (rng-seed :pointer)
     (results-ptr :pointer)))))


