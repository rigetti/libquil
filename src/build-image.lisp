(require '#:asdf)

;; libquil depends on systems that come from Quicklisp (cffi, bordeaux-threads,
;; ...). ASDF alone will not fetch those, so prefer Quicklisp when it is
;; available and fall back to plain ASDF for setups that vendor the
;; dependencies themselves.
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (and (null (find-package '#:quicklisp))
             (probe-file quicklisp-init))
    (load quicklisp-init)))

;; libquil intentionally redefines some of the alien callables that
;; SBCL-LIBRARIAN generates, in order to give them types SBCL-LIBRARIAN cannot
;; express yet (see the definition of quilc_compile_protoquil). SBCL signals a
;; continuable error for that; taking the CONTINUE restart installs the new
;; definition, which is what an interactive build does. Without this the build
;; drops into the debugger and cannot run unattended.
;;
;; The definitions are ordered so that libquil's override is installed last and
;; therefore wins; this applies equally when the systems are recompiled into FASL
;; bundles, so the whole build runs inside the handler.
(defmacro with-alien-redefinition-allowed (&body body)
  `(handler-bind ((error
                    (lambda (condition)
                      (let ((restart (find-restart 'continue condition)))
                        (when (and restart
                                   (search "redefine alien callable"
                                           (princ-to-string condition)))
                          (invoke-restart restart))))))
     ,@body))

(defun load-system (system)
  (with-alien-redefinition-allowed
    (if (find-package '#:quicklisp)
        (funcall (read-from-string "quicklisp:quickload") system)
        (asdf:load-system system))))

(load-system '#:sbcl-librarian)
(load-system '#:libquil)

(in-package #:libquil)

;;; Two aggregate libraries, because the C side and the Lisp side need different
;;; sets.
;;;
;;; The bindings we generate must cover only libquil's own APIs: the error, handle
;;; and diagnostic APIs already have C wrappers in libsbcl_librarian, and emitting
;;; them again would define those symbols twice.
(sbcl-librarian:define-aggregate-library libquil (:function-linkage "QUILC_API")
  quilc
  qvm)

;;; The runtime's own APIs. Their C wrappers live in libsbcl_librarian, and
;;; generating them here means the runtime can be built straight from this image;
;;; sbcl-librarian's own lib/generate-bindings.lisp would do it, but it pulls in
;;; swank and insists on saving a second core we would only discard.
;;;
;;; The name matters: it decides the generated file names, and entry_point.c
;;; includes sbcl_librarian.h.
(sbcl-librarian:define-aggregate-library sbcl-librarian
    (:function-linkage "LIBSBCL_LIBRARIAN_API")
  sbcl-librarian:diagnostics
  sbcl-librarian:environment
  sbcl-librarian:errors
  sbcl-librarian:handles
  sbcl-librarian:loader)

;;; The core has to export the Lisp side of everything the process will call,
;;; libquil's APIs and the runtime's alike: libsbcl_librarian's C wrapper for
;;; get_error_message dispatches to an alien callable that exists only if this core
;;; exports it.
(sbcl-librarian:define-aggregate-library libquil-core (:function-linkage "QUILC_API")
  quilc
  qvm
  sbcl-librarian:diagnostics
  sbcl-librarian:environment
  sbcl-librarian:errors
  sbcl-librarian:handles
  sbcl-librarian:loader)

;;; libquil ships a core rather than the FASL bundles that sbcl-librarian's
;;; CREATE-FASL-LIBRARY-CMAKE-PROJECT produces. FASL bundles are re-loaded into the
;;; image on every startup, which re-runs load-time code: cl-quil reads
;;; stdgates.quil through ASDF:SYSTEM-RELATIVE-PATHNAME at load time, so a FASL
;;; build only works where quilc's source tree is present at the path recorded when
;;; it was built. Saving a core evaluates that once, at build time, and bakes the
;;; result into the image.
;;;
;;; The init function is omitted: initialization is the runtime's job, done from a
;;; constructor when libsbcl_librarian is loaded.
(cl-user::with-alien-redefinition-allowed
  (sbcl-librarian:build-bindings libquil "." :omit-init-function t)
  (sbcl-librarian:build-bindings sbcl-librarian "runtime/" :omit-init-function t)
  (sbcl-librarian:build-core-and-die libquil-core "."))
