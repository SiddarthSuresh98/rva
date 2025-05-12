;;;;;;;;
;;; tests the stride load/store operations

.data
	s 8
	j 0 2 4 6 8 10 -6 -3 	; tricky stride
	d 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
	r 0
.text
	addi $5 $0 d 		; obtain pointer to data/result
	addi $6 $0 r
	addi $7 $0 j
	load $4 s($0)		; set vector length
	addv $17 $16 $16

;;; TMP TESTING ;;;
	addi $4 $0 0x0
	addv $17 $16 $16 	; clear vector
;;; END ;;;

	addv $18 $17 $16

	srdl $19 $7 $16 	; load fun stride
	rotv $19 $19 $5 	; mix it up
	srds $19 $6 $17

	srdl $20 $5 $17
	srds $20 $6 $16

	srdl $20 $5 $19
	srds $20 $6 $16

	nop
	nop
	nop
