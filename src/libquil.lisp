(in-package #:libquil)

(defun unpack-c-array-to-lisp-list (ptr len type)
  (loop :for i :below len
        :collect (cffi:mem-aref (sb-alien:alien-sap ptr) type i)))

(defun unpack-c-array-to-list-of-quil-program (ptr len)
  (loop :for i :below len
        :collect (sbcl-librarian::dereference-handle
                  (sb-alien::sap-alien
                   (cffi:mem-aref (sb-alien:alien-sap ptr) :pointer i)
                   (* t)))))

(defun unpack-maybe-nil-pointer (ptr type)
  (let ((sap (sb-alien:alien-sap ptr)))
    (unless (cffi:null-pointer-p sap)
      (cffi:mem-aref sap type))))

(defun unpack-maybe-nil-pointer-to-libquil-object (ptr)
  (let ((sap (sb-alien:alien-sap ptr)))
    (unless (cffi:null-pointer-p sap)
      (sbcl-librarian::dereference-handle
       (sb-alien::sap-alien
        (cffi:mem-aref sap :pointer)
        (* t))))))

(defun null-pointer-p (ptr)
  (cffi:null-pointer-p (sb-alien:alien-sap ptr)))

(sbcl-librarian:define-handle-type qvm-multishot-addresses "qvm_multishot_addresses")

(defvar *last-error* "")

(defun libquil-last-error ()
  "Returns the most recent error raised by quilc. The error is then cleared."
  (let ((last-error *last-error*))
    (setf *last-error* "") 
    last-error))

(sbcl-librarian:define-enum-type error-type "libquil_error_t"
  ("LIBQUIL_ERROR_SUCCESS" 0)
  ("LIBQUIL_ERROR_FAIL" 1))

(sbcl-librarian:define-error-map error-map error-type 0
  ((t (lambda (condition)
        (setf *last-error* (format nil "~a" condition))
        (return-from error-map 1)))))

(sbcl-librarian:define-api common (:error-map error-map
                                   :function-prefix "libquil_")
  (:type error-type)
  (:function
   (("error" libquil-last-error) :string ())))
