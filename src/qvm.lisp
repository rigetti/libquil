(in-package :libquil)

(defun qvm-version ()
  (qvm-app::handle-version))

(sbcl-librarian:define-handle-type qvm-multishot-result "qvm_multishot_result")

(defun qvm-multishot (compiled-quil addresses trials)
  "Executes COMPILED-QUIL on a pure-state QVM TRIALS numbers of times. At the end of each execution, the measurements for ADDRESSES are collected. The return value is a list of those measurements."
  (let* ((num-qubits (cl-quil.frontend::qubits-needed compiled-quil))
         (addresses (yason:parse addresses))
         (results (qvm-app::%perform-multishot
                   'qvm::pure-state compiled-quil num-qubits addresses trials nil nil)))
    results))

(defun qvm-multishot-result-get (multishot-result address-name shot-index result-pointer)
  (let* ((results (elt (gethash address-name multishot-result) shot-index)))
    (loop :for val :in results
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap result-pointer) :int i) val))))

(defun qvm-multishot-measure (compiled-quil qubits-ptr n-qubits trials results-ptr)
  (let ((qubits (loop :for i :below n-qubits :collect (cffi:mem-aref (sb-alien:alien-sap qubits-ptr) :int i))))
    (multiple-value-bind (compiled-quil relabeling)
                         (qvm-app::process-quil compiled-quil)
                         (let* ((num-qubits (cl-quil:qubits-needed compiled-quil))
                                (results (qvm-app::%perform-multishot-measure
                                          'qvm-app::pure-state
                                          compiled-quil
                                          num-qubits
                                          qubits
                                          trials
                                          relabeling)))
                           (loop :for trial :in results
                                 :for i :below trials :do
                                 (loop :for value :in trial
                                       :for j :from 0 :do
                                       (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :int (+ (* i (length trial)) j))
                                             value)))))))

(defun qvm-expectation (state-prep operators-ptr n-operators results-ptr)
  (let* ((operator-programs
           (loop :for i :below n-operators
                 :collect (sbcl-librarian::dereference-handle
                           (sb-alien::sap-alien
                            (cffi:mem-aref (sb-alien:alien-sap operators-ptr) :pointer i)
                            (* t)))))
         (num-qubits
           (loop :for p :in (cons state-prep operator-programs)
                 :maximize (cl-quil:qubits-needed p)))
         (expectations (qvm-app::%perform-expectation
                        'qvm-app::pure-state #'qvm-app::pure-state-expectation state-prep operator-programs num-qubits nil nil)))
    (loop :for expectation :in expectations
          :for i :below n-operators :do
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double i)
                  expectation))))

(defun qvm-wavefunction (program results-ptr)
  (let* ((num-qubits (cl-quil:qubits-needed program))
         (qvm (qvm-app::%execute-quil 'qvm-app::pure-state program num-qubits nil nil))
         (amplitudes (qvm::amplitudes qvm)))
    (loop :for amplitude :across amplitudes
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double (* i 2))
                  (realpart amplitude))
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double (1+ (* i 2)))
                  (imagpart amplitude)))))

(defun qvm-probabilities (program results-ptr)
  (let* ((num-qubits (cl-quil:qubits-needed program))
         (probabilities (multiple-value-bind (qvm probabilities)
                            (qvm-app::perform-probabilities 'qvm-app::pure-state program num-qubits)
                          probabilities)))
    (loop :for probability :across probabilities
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap results-ptr) :double i)
                  probability))))

(sbcl-librarian:define-api qvm (:error-map error-map :function-prefix "qvm_")
  (:literal "/* QVM types */")
  (:type qvm-multishot-result)
  (:literal "/* QVM functions */")
  (:function
   (("get_version_info" qvm-version)
    :string
    ())
   (("multishot" qvm-multishot)
    qvm-multishot-result
    ((program quil-program) (addresses :string) (trials :int)))
   (("multishot_result_get" qvm-multishot-result-get)
    :void
    ((qvm-result qvm-multishot-result) (region-name :string) (region-index :int) (result :pointer)))
   (("multishot_measure" qvm-multishot-measure)
    :void
    ((program quil-program) (qubits :pointer) (n-qubits :int) (trials :int) (result :pointer)))
   (("expectation" qvm-expectation)
    :void
    ((state-prep quil-program) (operators :pointer) (n-operators :int) (results-ptr :pointer)))
   (("wavefunction" qvm-wavefunction)
    :void
    ((program quil-program) (results-ptr :pointer)))
   (("probabilities" qvm-probabilities)
    :void
    ((program quil-program) (results-ptr :pointer)))))

