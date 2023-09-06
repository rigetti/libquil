(in-package :libquil)

(sbcl-librarian:define-handle-type quil-program "quil_program")
(sbcl-librarian:define-handle-type chip-specification "chip_specification")

(defun quilc-get-version-info (version-ptr githash-ptr)
  (let ((version quilc::+QUILC-VERSION+)
        (githash quilc::+GIT-HASH+))
    (loop :for char :across version
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap version-ptr) :char i)
                  (char-code char)))
    (loop :for char :across githash
          :for i :from 0 :do
            (setf (cffi:mem-aref (sb-alien:alien-sap githash-ptr) :char i)
                  (char-code char)))))

(defun compile-protoquil (parsed-program chip-specification)
  (let ((compiled-program (cl-quil::compiler-hook parsed-program chip-specification :protoquil t)))
    (cl-quil.frontend::transform 'cl-quil.frontend::process-protoquil compiled-program)
    compiled-program))

(defun program-to-string (program)
  (with-output-to-string (s)
    (cl-quil.frontend:print-parsed-program program s)))

(defun parse-chip-spec-isa-json (isa-json)
  (time (cl-quil::qpu-hash-table-to-chip-specification (yason:parse isa-json))))

(sbcl-librarian:define-api quilc (:error-map error-map
                                  :function-prefix "quilc_")
  (:literal "/* Quilc types */")
  (:type quil-program chip-specification)
  (:literal "/* Quilc functions */")
  (:function
   (("get_version_info" quilc-get-version-info) :void ((version :pointer) (githash :pointer)))
   (("parse_quil" cl-quil.frontend:safely-parse-quil) quil-program ((source :string)))
   (("print_program" cl-quil.frontend:print-parsed-program) :void ((program quil-program)))
   (("compile_quil" cl-quil:compiler-hook) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("compile_protoquil" compile-protoquil) quil-program ((program quil-program) (chip-spec chip-specification)))
   (("build_nq_linear_chip" cl-quil::build-nq-linear-chip) chip-specification ((n :int)))
   (("chip_spec_from_isa_descriptor" quilc::lookup-isa-descriptor-for-name) chip-specification ((descriptor :string)))
   (("print_chip_spec" cl-quil::debug-print-chip-spec) :void ((chip-spec chip-specification)))
   (("parse_chip_spec_isa_json" parse-chip-spec-isa-json) chip-specification ((isa-json :string)))
   (("program_string" program-to-string) :string ((program quil-program)))))
