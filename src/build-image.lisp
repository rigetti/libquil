(require '#:asdf)

;; libquil depends on systems that come from Quicklisp (cffi, bordeaux-threads,
;; ...). ASDF alone will not fetch those, so prefer Quicklisp when it is
;; available and fall back to plain ASDF for setups that vendor the
;; dependencies themselves.
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (and (null (find-package '#:quicklisp))
             (probe-file quicklisp-init))
    (load quicklisp-init)))

(defun load-system (system)
  ;; libquil intentionally redefines some of the alien callables that
  ;; SBCL-LIBRARIAN generates, in order to give them types SBCL-LIBRARIAN cannot
  ;; express yet (see the definition of quilc_compile_protoquil). SBCL signals a
  ;; continuable error for that; taking the CONTINUE restart installs the new
  ;; definition, which is what an interactive build does. Without this the build
  ;; drops into the debugger and cannot run unattended.
  (handler-bind ((error
                   (lambda (condition)
                     (let ((restart (find-restart 'continue condition)))
                       (when (and restart
                                  (search "redefine alien callable"
                                          (princ-to-string condition)))
                         (invoke-restart restart))))))
    (if (find-package '#:quicklisp)
        (funcall (read-from-string "quicklisp:quickload") system)
        (asdf:load-system system))))

(load-system '#:sbcl-librarian)
(load-system '#:libquil)

(in-package #:libquil)

(sbcl-librarian:define-aggregate-library libquil (:function-linkage "QUILC_API")
  common
  quilc
  qvm
  sbcl-librarian:handles)

(sbcl-librarian:build-bindings libquil "." :initialize-lisp-args '("--dynamic-space-size" "8192"))
(sbcl-librarian:build-core-and-die libquil ".")
