(in-package #:libquil)

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
