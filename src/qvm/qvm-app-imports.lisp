(in-package :libquil)

(eval-when (:compile-toplevel :load-toplevel)
  (alexandria:define-constant +QVM-VERSION+
      (system-version '#:qvm)
    :test #'string=
    :documentation "The version of the QVM itself.")

  (alexandria:define-constant +QVM-GIT-HASH+
      (git-hash '#:qvm-app)
    :test #'string= :documentation "The git hash of the QVM repo.")
  )

(defun qubits-in-range-p (qam qubits)
  "Are all qubits in the list QUBITS in range of the QAM?"
  (loop :with maxq := (1- (qvm:number-of-qubits qam))
        :for q :in (remove ':unused-qubit qubits)
        :always (<= 0 q maxq)))

(defun parallel-measure (qvm &optional qubits)
  (cond
    ;; Fast path: measure all of the qubits. Note that we check for
    ;; "all" by checking that we have n distinct qubits.
    ((= (qvm:number-of-qubits qvm)
        (count-if-not (lambda (q) (eql q ':unused-qubit))
                      (remove-duplicates qubits)))
     (let ((bits (nth-value 1 (qvm:measure-all qvm))))
       (loop :for q :in qubits
             :if (eql q ':unused-qubit)
               :collect 0
             :else
               :collect (nth q bits))))
    ;; Slow path. Measure only some of the qubits.
    ;;
    ;; XXX: Debate whether we actually shouldn't just MEASURE-ALL
    ;; and take only some of the qubits. This would have
    ;; repercussions on a persistent QVM.
    (t
     (loop :for q :in qubits
           :if (eql q ':unused-qubit)
             :collect 0
           :else
             :collect (nth-value 1 (qvm:measure qvm q))))))

(defvar *qubit-limit* nil)              ; Maximum no. of qubits.
(defvar *num-workers* nil)
(defvar *time-limit* nil)
(defvar *allocation-description* 'qvm:lisp-allocation "Default allocation description.")
(global-vars:define-global-var **default-allocation**
    (lambda (n) (make-instance *allocation-description* :length n)))

(defun throw-error-if-over-allocated (num-qubits)
  "Throws an error if the number of qubits requested exceeds the max (defined from command line parameter --qubit-limit)."
  (when (and (integerp *qubit-limit*) (> num-qubits *qubit-limit*))
    (error "~D qubits were requested, but the QVM ~
             is limited to ~D qubits." num-qubits *qubit-limit*)))

(defun make-wavefunction (length)
  (multiple-value-bind (vec fin)
      (qvm:allocate-vector (funcall **default-allocation** length))
    (qvm::bring-to-zero-state vec)
    (values vec fin)))

(defun extract-kraus-ops (quil)
  "Iterate over the instructions in QUIL, collect all PRAGMA ADD-KRAUS
statements and convert them into a hash-table with keys
'(<gatename> (<qubit1>+)) and as values a list of MAGICL matrices.
"
  (let ((kraus-ops (make-hash-table :test 'equal)))
    (loop :for instr :across (cl-quil:parsed-program-executable-code quil)
          :when (typep instr 'cl-quil::pragma-add-kraus)
            :do (let ((existing-ops (gethash (list (cl-quil::pragma-operator-name instr)
                                                   (cl-quil::pragma-qubit-arguments instr))
                                             kraus-ops))
                      (d (expt 2 (length (cl-quil::pragma-qubit-arguments instr)))))
                  (setf (gethash (list (cl-quil::pragma-operator-name instr)
                                       (cl-quil::pragma-qubit-arguments instr))
                                 kraus-ops)
                        (append existing-ops
                                (list (magicl:from-list
                                       (cl-quil::pragma-matrix-entries instr)
                                       (list d d)
                                       :type '(complex double-float))))))
                ;; change to no-op to prevent the QVM from logging
                ;; warnings about these pragmas
                (change-class instr 'cl-quil:no-operation))
    kraus-ops))


(defun extract-readout-povms (quil)
  "Iterate over the instructions in QUIL, collect all PRAGMA
READOUT-POVM statements and convert them into a hash-table with keys
given by the qubit id and as values MAGICL matrices that encode the
single qubit readout povm.
"
  (let ((readout-povms (make-hash-table :test 'equal)))
    (loop :for instr :across (cl-quil:parsed-program-executable-code quil)
          :when (typep instr 'cl-quil::pragma-readout-povm)
            :do (setf (gethash (cl-quil::pragma-qubit-index instr) readout-povms)
                      (cl-quil::pragma-matrix-entries instr))
                ;; change to no-op to prevent the QVM from logging
                ;; warnings about these pragmas
                (change-class instr 'cl-quil:no-operation))
    readout-povms))

(defun make-appropriate-qvm (quil num-qubits gate-noise measurement-noise)
  "Determine if a QVM:NOISY-QVM or QVM:DEPOLARIZING-QVM is needed."
  (throw-error-if-over-allocated num-qubits)
  (let* ((kraus-ops (extract-kraus-ops quil))
         (readout-povms (extract-readout-povms quil))
         (need-noisy-qvm (or (plusp
                              (hash-table-count kraus-ops))
                             (plusp
                              (hash-table-count readout-povms))))
         (classical-memory-model (qvm::memory-descriptors-to-qvm-memory-model
                                  (cl-quil:parsed-program-memory-definitions quil))))
    (cond
      ((and (null gate-noise)
            (null measurement-noise)
            (not need-noisy-qvm))
       (qvm:make-qvm num-qubits :classical-memory-model classical-memory-model
                                :allocation (funcall **default-allocation**
                                                     (expt 2 num-qubits))))
      (need-noisy-qvm
       (when (not (and (null gate-noise)
                       (null measurement-noise)))
         (error "Cannot simultaneously support Pauli error model and Kraus channels."))

       (multiple-value-bind (amps fin) (make-wavefunction (expt 2 num-qubits))
         (let* ((state (make-instance 'qvm:pure-state :num-qubits num-qubits
                                                      :amplitudes amps))
                (q
                  (make-instance 'qvm:noisy-qvm
                                 :number-of-qubits num-qubits
                                 :noisy-gate-definitions kraus-ops
                                 :readout-povms readout-povms
                                 :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                            :classical-memory-model
                                                                            classical-memory-model)
                                 :state state)))
           (when fin (tg:finalize state fin))
           q)))
      (t
       (let ((gate-noise (or gate-noise '(0.0 0.0 0.0)))
             (measurement-noise (or measurement-noise '(0.0 0.0 0.0))))
         (multiple-value-bind (amps fin) (make-wavefunction (expt 2 num-qubits))
           (let* ((state (make-instance 'qvm:pure-state :num-qubits num-qubits
                                                        :amplitudes amps))
                  (q (make-instance 'qvm:depolarizing-qvm
                                    :number-of-qubits num-qubits
                                    :classical-memory-subsystem (make-instance 'qvm:classical-memory-subsystem
                                                                               :classical-memory-model
                                                                               classical-memory-model)
                                    :x (elt gate-noise 0)
                                    :y (elt gate-noise 1)
                                    :z (elt gate-noise 2)
                                    :measure-x (elt measurement-noise 0)
                                    :measure-y (elt measurement-noise 1)
                                    :measure-z (elt measurement-noise 2)
                                    :state state)))
             (when fin (tg:finalize state fin))
             q)))))))

(defmacro with-timeout (&body body)
  (let ((f (gensym "TIME-LIMITED-BODY-")))
    `(flet ((,f () ,@body))
       (declare (dynamic-extent (function ,f)))
       (if (null *time-limit*)
           (,f)
           (bt:with-timeout (*time-limit*)
             (,f))))))

(defun %perform-multishot-measure (quil num-qubits qubits num-trials relabeling)
  (check-type quil cl-quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type qubits alexandria:proper-list)
  (check-type relabeling (or null (vector unsigned-byte)))
  (assert (every (alexandria:conjoin #'integerp (complement #'minusp)) qubits))

  (when (or (null qubits) (zerop num-trials))
    (return-from %perform-multishot-measure nil))

  ;; Relabel the qubits according to RELABELING. This is O(N^2), but N
  ;; will always be less than 40 or so.
  (when relabeling
    (setf qubits
          (loop :for qubit :in qubits
                :collect (or (position qubit relabeling) ':unused-qubit)))
    (setf num-qubits
          (max num-qubits
               (1+ (reduce #'max
                           (remove ':unused-qubit qubits)

                           ;; Specify initial value in case all qubits
                           ;; are unused. An initial value -1 is
                           ;; incremented to 0 by outer (1+ ...) call
                           :initial-value -1)))))

  (let ((qvm (make-appropriate-qvm quil num-qubits nil nil)))
    ;; Check that we've asked for sensible qubits.
    (assert (qubits-in-range-p qvm qubits) ()
            "The provided qubits ~S to a multishot measure are out ~
             of range for the given QVM, which only has ~D qubit~:P."
            qubits
            (qvm:number-of-qubits qvm))
    ;; Make the initial state.
    (qvm:load-program qvm quil)
    (with-timeout
        (qvm:run qvm))
    (let ((prepared-wf
            (qvm:copy-wavefunction (qvm::amplitudes qvm)))
          (first-time t))
      (flet ((reload (qvm)
               (unless first-time
                 (qvm:copy-wavefunction prepared-wf (qvm::amplitudes qvm)))
               (setf first-time nil)))
        ;; Do the parallel measurements
        (loop :repeat num-trials
              :collect (progn
                         (reload qvm)
                         (parallel-measure qvm qubits)))))))

(defun process-quil (quil)
  "Prepare the PARSED-PROGRAM QUIL for more efficient execution. Currently this only includes remapping the qubits to a minimal sequential set from 0 to (num-qubits-used - 1). Return two values: the processed Quil code and the mapping vector.

The mapping vector V specifies that the qubit as specified in the program V[i] has been mapped to qubit i."
  (let* ((mapping (cl-quil::compute-qubit-mapping quil))
         (trivial-mapping-p
           (loop :for x :across mapping
                 :for i :from 0
                 :always (= x i))))
    (unless trivial-mapping-p
      (cl-quil::transform 'cl-quil::compress-qubits quil))
    (values quil mapping)))

(defun valid-address-query-p (addresses)
  (cond
    ((not (hash-table-p addresses)) nil)
    (t
     (maphash (lambda (k v)
                (unless (and (stringp k)
                             (or (eq t v)
                                 (and (alexandria:proper-list-p v)
                                      (every #'integerp v)
                                      (notany #'minusp v))))
                  (return-from valid-address-query-p nil)))
              addresses)
     t)))

(defun collect-result-data (qvm addresses results)
  (maphash (lambda (name indexes)
             (cond
               ;; Give everything back.
               ((eq indexes t)
                (loop :with mv := (gethash name (qvm::classical-memories qvm))
                      :for idx :below (qvm::memory-view-length mv)
                      :collect (qvm:memory-ref qvm name idx) :into mem
                      :finally (push mem (gethash name results))))
               ;; Give only some things back.
               ((alexandria:proper-list-p indexes)
                (loop :for idx :in indexes
                      :collect (qvm:memory-ref qvm name idx) :into mem
                      :finally (push mem (gethash name results))))
               (t
                (error "Invalid multishot address query for memory named ~S." name))))
           addresses)
  results)

(defun %perform-multishot (quil num-qubits addresses num-trials gate-noise measurement-noise)
  (check-type quil cl-quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type num-trials (integer 0))
  (check-type addresses hash-table)
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (valid-address-query-p addresses) ()
          "Detected invalid address query in multishot experiment. The ~
           requested addresses should be a JSON object whose keys are ~
           DECLAREd memory names, and whose values are either the true ~
           value to request all memory, or a list of non-negative integer ~
           indexes to request some memory.")
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  ;; Bail out early if there's no work to actually do.
  (when (or (zerop (hash-table-count addresses))
            (zerop num-trials)
            (loop :for v :being :the :hash-values :of addresses
                  :always (null v)))
    (return-from %perform-multishot (load-time-value (make-hash-table) t)))

  (let ((qvm (make-appropriate-qvm quil num-qubits gate-noise measurement-noise))
        (trial-results (make-hash-table :test 'equal
                                        :size (hash-table-count addresses))))
    (qvm:load-program qvm quil :supersede-memory-subsystem t)
    (dotimes (trial num-trials)
      ;; Reset the program counter.
      (setf (qvm::pc qvm) 0)

      ;; Reset the amplitudes, but only if running more than one trial.
      (unless (= 1 num-trials)
        ;; Reset the amplitudes.
        (qvm::reset-quantum-state qvm))

      ;; Run the program.
      (with-timeout (qvm:run qvm))

      ;; Collect all of the memory that the user requests.
      (collect-result-data qvm addresses trial-results))
    
    ;; We collected everything in reverse. So, reverse that.
    (maphash (lambda (k v)
               (setf (gethash k trial-results) (nreverse v)))
             trial-results)
    trial-results))

(defun %perform-expectation (expectation-op state-prep operators num-qubits gate-noise measurement-noise)
  (check-type state-prep cl-quil:parsed-program)
  (dolist (o operators) (check-type o cl-quil:parsed-program))
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  ;; If we have nothing to compute the expectation of, then return
  ;; nothing.
  (when (null operators)
    (return-from %perform-expectation '()))

  ;; Otherwise, go about business.
  (let ((qvm (make-appropriate-qvm state-prep num-qubits gate-noise measurement-noise)))
    ;; Make the initial state.
    (qvm:load-program qvm state-prep)
    (qvm:run qvm)
    
    (let ((prepared-wf
            (qvm:copy-wavefunction (qvm::amplitudes qvm)))
          (first-time t))
      ;; Compute the expectations of the operators.
      (loop :for i :from 1
            :for op :in operators
            :collect (let (expectation)
                       (setf expectation (funcall expectation-op qvm prepared-wf op first-time))
                       (setf first-time nil)
                       (assert (< (abs (imagpart expectation)) 1e-14))
                       (realpart expectation))))))

(defun pure-state-expectation (qvm prepared-state op &optional first-time)
  (flet ((inner-product (a b)
           (declare (type qvm::quantum-state a b))
           (loop :for ai :of-type qvm::cflonum :across a
                 :for bi :of-type qvm::cflonum :across b
                 :sum (* (conjugate ai) bi))))
    (unless first-time
      (qvm:copy-wavefunction prepared-state (qvm::amplitudes qvm)))
    (qvm:load-program qvm op)
    (qvm:run qvm)
    (inner-product prepared-state
                   (qvm::amplitudes qvm))))

(defun %execute-quil (quil num-qubits gate-noise measurement-noise)
  (check-type quil cl-quil:parsed-program)
  (check-type num-qubits (integer 0))
  (check-type gate-noise (or null alexandria:proper-list))
  (check-type measurement-noise (or null alexandria:proper-list))
  (assert (and (or (null gate-noise)
                   (= 3 (length gate-noise)))
               (every #'realp gate-noise)))
  (assert (and (or (null measurement-noise)
                   (= 3 (length measurement-noise)))
               (every #'realp measurement-noise)))

  (let ((qvm (make-appropriate-qvm quil num-qubits gate-noise measurement-noise)))
    (qvm:load-program qvm quil)
    (qvm:run qvm)
    qvm))

(defun %perform-probabilities (quil num-qubits &key gate-noise measurement-noise)
  (let* ((qvm (%execute-quil quil num-qubits gate-noise measurement-noise))
         (amplitudes (qvm::amplitudes qvm))
         (probabilities (make-array (length amplitudes) :element-type 'qvm:flonum)))
    (map-into probabilities #'qvm::probability (qvm::amplitudes qvm))
    (values qvm
            probabilities)))
