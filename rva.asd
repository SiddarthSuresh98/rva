(defsystem "rva"
  ;; :author ""
  ;; :license ""
  :version "0.1"
  :homepage "https://github.com/bdunahu/rva"
  :source-control (:git "git@github.com:bdunahu/rva.git")
  :depends-on ("uiop")
  :components ((:module "src"
	        :serial t
		:components ((:file "lex")
			     (:file "rva"))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "rva/tests")))
  :build-operation "program-op"
  :build-pathname "bin/rva"
  :entry-point "rva::main")

(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
