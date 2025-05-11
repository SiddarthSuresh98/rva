;;;;;;;;
;;; tests the ALU vector extension

.data
	s 4
.text
	load $4 s($0) 		; set the vector length register

LOOP:
	cev $17 $16
	beq END

	addv $17 $16 $17
	addv $17 $16 $17
	mulv $17 $17 $17
	subv $17 $17 $16
	subv $17 $17 $16
	subv $17 $17 $16
	jrl LOOP
END:
	nop
	nop
	nop
