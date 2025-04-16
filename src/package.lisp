;; Assembler for the RISC-V[ECTOR] mini-ISA.
;; Copyright (C) 2025

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defpackage #:rva
  (:use #:cl)
  (:export #:main))

(defpackage #:util
  (:use #:cl)
  (:export #:asm-extension?
           #:format-as-binary
           #:generate-file-name
           #:word-to-bytes
           #:insert-in-middle
           #:iota
           #:riffle
           #:add-variable
           #:add-label
           #:get-variable
           #:get-label
           #:r-type
           #:i-type
           #:j-type))

(defpackage #:parse
  (:use #:cl)
  (:export #:str->ast
           #:line-number))

(defpackage #:emit
  (:use #:cl)
  (:export #:emit
           #:ast->str))
