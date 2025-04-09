#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(asdf:defsystem #:rva
  ;; :author ""
  ;; :license ""
  :version "0.3"
  :homepage "https://github.com/bdunahu/rva"
  :description "Assembler for the RISC-V[ECTOR] mini-ISA."
  :source-control (:git "git@github.com:bdunahu/rva.git")
  :depends-on (:uiop
               :clingon
	       :esrap)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "lex")
                             (:file "parse")
                             (:file "main"))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "rva/tests")))
  :build-operation "program-op"
  :build-pathname "bin/rva"
  :entry-point "rva:main")

(asdf:defsystem #:rva/tests
  ;; :author ""
  ;; :license ""
  :description "rva's test suite"
  :depends-on (:rva
               :fiveam)
  :components ((:module "t"
                :serial t
                :components ((:file "package")
                             (:file "main")
                             (:file "util")
                             (:file "lex")
                             (:file "parse"))))
  :perform (test-op (o s) (uiop:symbol-call :rva-tests :test-rva)))

(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
