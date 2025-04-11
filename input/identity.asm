;;;;;;;;
;;; performs a matrix multiply on the 4x4 square matrices
;;; m1 and m2,
;;; the result of which is placed into
;;; r,
;;; in this case, the result is the identity
;;; does not use designated vector instructions

.data
	m1 01 00 -1 00 00 01 00 -1 00 00 01 00 00 00 00 01
	m2 01 00 01 00 00 01 00 01 00 00 01 00 00 00 00 01
	r  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
.text
	addi $8  $0 0x4	; dimensions
	addi $9  $0 0x0	; tracks the rows
	addi $10 $0 0x0	; tracks the columns
	jrl  ROWCOND

ROW:
	jrl  COLCOND
COL:
	addi $11 $0 0x0	; tracks the element being added
	addi $12 $0 0x0	; the accumulated result to place into $9,$10

	mul  $7 $9 $8	; setup the index into m1
	addi $5 $7 m1

	addi $6 $0 m2	; setup the index into m2
	add  $6 $6 $10

	jrl  ELECOND
ELE:
	add  $5 $5 $11	; increment m1

	mul  $13 $11 $8 ; increment m2
	add  $6 $6 $13

	load $13 0($5)	; retrieve and accumulate
	load $14 0($6)
	add  $12 $14 $13

	addi $11 $11 0x1
ELECOND:
	cmp  $8 $11
	bgt  ELE
	addi $10 $10 0x1
COLCOND:
	cmp  $8 $10
	bgt  COL

	add  $7 $7 $10	; setup the index into r
	addi $7 $7 r

	store $12 0($7)

	addi $9 $9 0x1
ROWCOND:
	cmp  $8 $9
	bgt  ROW
