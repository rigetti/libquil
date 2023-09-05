(require '#:asdf)

(asdf:load-system '#:sbcl-librarian)
(asdf:load-system '#:libquil)

(in-package #:libquil)

(sbcl-librarian:define-aggregate-library libquil (:function-linkage "QUILC_API")
  common
  quilc
  qvm
  sbcl-librarian:handles)

(sbcl-librarian:build-bindings libquil "." :initialize-lisp-args '("--dynamic-space-size" "8192"))
(sbcl-librarian:build-core-and-die libquil ".")
