;;;;;;;;
;;; makes use of a designated function to add two numbers together
;;; uses a stack-based paradigm with a base pointer to manage argument passing

.data
	answer 0

.text
MAIN:
	addi $5 $2 0x0 	; establish frame pointer

	addi $6 $0 -11

	push $6
	jal SUB23
	pop $6
	store $6 answer($0)
	nop
	nop
	nop
	quot $0 $0 $0
SUB23:
	push $5		; push old frame pointer
	addi $5 $2 0x0
	subi $2 $2 0x1

	addi $6 $0 -23
	store $6 -1($5)

	load $7 +1($5)	; access argument
	load $6 -1($5)

	add  $6 $6 $7
	push $1		; save off our old ret
	push $6
	jal ADD76
	pop $6		; retrieve and pass along
	pop $1		; restore our old ret
	store $6 +1($5)

	addi $2 $5 0x0	; restore stack pointer
	pop $5		; restore frame pointer
	ret
ADD76:
	push $5
	addi $5 $2 0x0
	subi $2 $2 0x1

	addi $6 $0 +76
	store $6 -1($5)

	load $7 +1($5)	; access argument
	load $6 -1($5)

	add  $6 $6 $7
	store $6 +1($5)

	addi $2 $5 0x0
	pop $5
	ret
	nop
	nop
	nop
	nop
