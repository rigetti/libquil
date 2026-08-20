(in-package #:libquil)

(defun foreign-alloc-and-set-string (c-ptr s)
  (let* ((ptr (cffi:foreign-alloc :string :initial-element s)))
    (setf (cffi:mem-ref (sb-alien:alien-sap c-ptr) :pointer)
          (sb-alien:alien-sap (sb-alien:deref (sb-alien:sap-alien ptr (* (* t))))))))

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

;;; Error reporting comes from SBCL-LIBRARIAN's built-in ERRORS api, which
;;; provides the `lisp_err_t' type, `get_error_message', and `enable_backtrace'.
;;; DEFINE-API always uses SBCL-LIBRARIAN's DEFAULT-ERROR-MAP, which records the
;;; condition into that message.
;;;
;;; We redefine that map for libquil's own APIs. The stock one classifies any
;;; plain CL:ERROR as an internal bug (LISP_ERR_BUG), attaching a backtrace and an
;;; "Internal lisp bug:" prefix. Almost everything libquil signals is a user error
;;; -- malformed Quil, an unknown memory region, an unsupported instruction -- so
;;; the stock mapping would report ordinary bad input as a libquil bug. Mapping
;;; CL:ERROR to LISP_ERR_FAILURE keeps the message clean and matches how libquil
;;; behaved before it adopted the built-in error handling.
;;;
;;; WRAP-ERROR-HANDLING is consulted when a callable is compiled, so this affects
;;; only the callables compiled after it -- libquil's own. The APIs already
;;; compiled into libsbcl_librarian keep the stock behaviour.
(sbcl-librarian:define-error-map sbcl-librarian::default-error-map
    sbcl-librarian::error-type (:no-error 0 :fatal-error 3)
  ;; Handle T, not CL:ERROR. cl-quil signals conditions that are not subtypes of
  ;; ERROR -- INVALID-INSTRUCTION-CONDITION, raised for an unrecognized
  ;; instruction, has no supertype at all -- so a handler bound to CL:ERROR lets
  ;; them escape to the debugger and hang the calling process. Warnings are
  ;; passed over first so they do not abort the call.
  ((cl:warning #'cl:continue)
   (t (lambda (condition)
        (setf sbcl-librarian::*error-message* (format nil "~a" condition))
        (return-from sbcl-librarian::default-error-map 1)))))
