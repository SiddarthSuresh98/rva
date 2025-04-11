;;;;;;;;
;;; makes use of a designated function to add two numbers together
;;; uses a stack-based paradigm with a base pointer to manage argument passing
;;; TODO this file is incomplete due to remaining ISA subroutine design issues

.data
	answer 0

.text
MAIN:
	addi $5 $2 0x0 	; establish frame pointer

	addi $6 $0 -11

	push $6
	;; jal SUB23
	store $6 answer($0)
SUB23:
	push $5		; push old frame pointer
	addi $5 $2 0x0
	subi $2 $2 0x4

	addi $6 $0 -23
	store $6 -4($5)

	load $7 +4($5)	; access argument
	load $6 -4($5)

	add  $6 $6 $7
	push $6
	;; jal ADD76
	pop $6		; retrieve and pass along
	store $6 +4($5)

	addi $2 $5 0x0	; restore stack pointer
	pop $5		; restore frame pointer
	;; ret
ADD76:
	push $5
	addi $5 $2 0x0
	subi $2 $2 0x4

	addi $6 $0 +76
	store $6 -4($5)

	load $7 +4($5)	; access argument
	load $6 -4($5)

	add  $6 $6 $7
	store $6 +4($5)

	addi $2 $5 0x0
	pop $5
	;; ret
