(in-package #:rva)

(defparameter *banner*
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
    :description "run the lexer, but stop before parsing"
    :long-name "lex"
    :short-name #\l
    :required nil
    :key :lex)
   (clingon:make-option
    :flag
    :description "run the parser, but stop before emission"
    :long-name "parse"
    :short-name #\p
    :required nil
    :key :parse)))

(defun driver (cmd)
  "Reads in a file and directs lexing, parsing, and binary emission."
  (print-splash)
  (let* ((args (clingon:command-arguments cmd))
         (file (car args))
         (parse? (not (clingon:getopt cmd :lex)))
         (emit? (not (clingon:getopt cmd :parse))))
    (cond
      ;; complain about num arguments
      ((/= (length args) 1) (error "Wrong number of arguments."))
      ((not (util:asm-extension? file))
       (error "The file is not an asm source code file."))
      (t (let ((tokens (lex:file->tokens file)))
           (if tokens
               (format t "~a" tokens)
               (error "The file does not exist, or it could not be opened."))
           (format t "Nitimur in Vetitum~%"))))))


(defun cli/command ()
  "Returns a clingon command."
  (clingon:make-command
   :name "rva"
   :description "generates a binary compatible with the RISC V[ECTOR] simulator"
   :usage "[options] file"
   :version(asdf:component-version
            (asdf:find-system "rva" nil))
   :options (cli/options)
   :handler #'driver))

(defun main ()
  (clingon:run (cli/command)))
