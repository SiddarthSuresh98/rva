(in-package #:rva)

(defvar *banner*
  "      _/_/                                  _/_/
     _/                                      _/
    _/    _/  _/_/  _/      _/    _/_/_/    _/
   _/    _/_/      _/      _/  _/    _/    _/
  _/    _/          _/  _/    _/    _/    _/
 _/    _/            _/        _/_/_/    _/
_/_/                                  _/_/  "
  "Stylized ASCII logo.")

(defun print-splash ()
  "Prints a pretty splash-screen."
  (format t "~a~%" *banner*))

(defun cli/options ()
  "Returns options for the `rva' assembler."
  (list
   (clingon:make-option
    :flag
    :description "run the parser, but stop before emission"
    :long-name "parse"
    :short-name #\p
    :required nil
    :key :parse)
   (clingon:make-option
    :flag
    :description "run the emitter, but stop before writing"
    :long-name "emit"
    :short-name #\e
    :required nil
    :key :emit)))

(defun postprocess (out file)
  "Given OUT, a list of words, writes the raw bytes to FILE."
  (let ((file (util:generate-file-name file))
        (bytes (alexandria:flatten
                (mapcar (lambda (x)
                          (util:word-to-bytes (parse-integer x :radix 2)))
                        out))))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (loop for byte in bytes do (write-byte byte stream)))
    (format t "File written! Check ~a~%" file)))

(defun driver (cmd)
  "Reads in a file and directs lexing, parsing, and binary emission."
  (print-splash)
  (let* ((args (clingon:command-arguments cmd))
         (file (car args))
         (emit? (not (clingon:getopt cmd :parse)))
         (write? (not (clingon:getopt cmd :emit))))
    (cond
      ((/= (length args) 1) (error "Wrong number of arguments.~%"))
      ((not (util:asm-extension? file))
       (error "The file is not an asm source code file.~%"))
      (t (let ((str (uiop:read-file-string file)))
           (if str
               (let ((ast (esrap:parse 'parse::str->ast (string-upcase str))))
                 (if emit?
                     (let ((words (emit:emit ast)))
                       (if write?
                           (postprocess words file)
                           (format t "Emission successfull, got: ~%~a~%" words)))
                     (format t "Parse successful, got:~%~a~%" (emit:ast->str ast))))
               (error "The file does not exist, or it could not be opened.~%")))))))

(defun cli/command ()
  "Returns a clingon command."
  (clingon:make-command
   :name "rva"
   :description "generates a binary compatible with the RISC V[ECTOR] simulator"
   :usage "[options] file"
   :version (asdf:component-version
            (asdf:find-system "rva" nil))
   :options (cli/options)
   :handler #'driver))

(defun main ()
  (clingon:run (cli/command)))
