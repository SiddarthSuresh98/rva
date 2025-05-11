;;;;;;;;
;;; tests the over/underflow conditions on various operations

.data
	n1 -1
	space1 0 0 0 0 0 0 0 	; space is added to load this as a vector
	max 0x7FFFFFFF
	space2 0 0 0 0 0 0 0
	min 0x80000000
	vSiz 8

.text
	load $4 vSiz($0)
	load $5 max($0)
	load $6 min($0)
	loadv $17 max($0)
	loadv $18 min($0)
	loadv $19 n1($0)
	addi $7 $0 1
	addi $8 $0 -1
	jrl ADDROVER

WIN:
	nop
	nop
	nop
	quot $0 $0 $0

ADDROVER:
	add $0 $5 $7
	bof SUBRUNDER
	jrl DIE

SUBRUNDER:
	sub $0 $6 $7
	buf MULROK
	jrl DIE

MULROK:
	mul $0 $5 $7
	buf DIE
	jrl MULVROVER

MULVROVER:
	mulv $16 $18 $19
	bof DIVVIOVER
	jrl DIE

DIVVIOVER:
	divv $15 $18 $19
	bof WIN
	jrl DIE

DIE:
	nop
	nop
	nop
