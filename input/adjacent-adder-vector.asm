;;;;;;;;
;;; adds adjacent elements of a 4-element vector together,
;;; storing the result in place. Uses the vector type.

.data
	arr 1 2 3 4
	s 3
.text
	load $4 s($0) 	; set the vector-length register
	addi $5 $0 arr
	loadv $16 0($5)
	addi $5 $5 0x1
	loadv $17 0($6)
	addv $16 $16 $17
	storev $16 arr($0)
	nop
	nop
	nop
