;;;; libquil.asd

(asdf:defsystem #:libquil
  :description "Shared library API for Quilc and QVM"
  :author "Mark Skilbeck <mark.skilbeck@rigetti.com>"
  :version (:read-file-form "VERSION.txt")
  :license "Apache License 2.0 (See LICENSE.txt)"
  :pathname "src/"
  :depends-on (#:cffi
               #:bordeaux-threads
               #:cl-quil
               #:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "libquil")
               (:module "quilc"
                :serial t
                :components ((:file "quilc-imports")
                             (:file "compile")
                             (:file "api")))
               (:module "qvm"
                :serial t
                :components ((:file "qvm-app-imports")
                             (:file "api")))))
