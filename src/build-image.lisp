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
  (if (find-package '#:quicklisp)
      (funcall (read-from-string "quicklisp:quickload") system)
      (asdf:load-system system)))

;; The BLAS/LAPACK backend is magicl's to choose. It searches for OpenBLAS first,
;; and refuses to load a backend that is missing routines quilc calls or that
;; computes incorrect eigenvectors, so a bad one fails this build with a message
;; naming the library rather than producing an artifact that is quietly wrong.
;;
;; Whatever magicl loads is recorded in the core by SBCL and reopened at startup,
;; so the choice is still baked into the artifact rather than left to the loader.

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
(sbcl-librarian:build-bindings libquil "." :omit-init-function t)
(sbcl-librarian:build-bindings sbcl-librarian "runtime/" :omit-init-function t)
(sbcl-librarian:build-core-and-die libquil-core ".")
