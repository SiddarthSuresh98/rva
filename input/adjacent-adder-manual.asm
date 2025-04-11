;;;;;;;;
;;; adds adjacent elements of a 4-element vector together,
;;; storing the result in place.
;;; Does not make use of variables.

.data
	;; empty
.text
        addi $2 $0 0x200
        addi $5 $0 0x1
        store $5 0($2)
        addi $5 $0 0x2
        store $5 1($2)
        addi $5 $0 0x3
        store $5 2($2)
        addi $5 $0 0x4
        store $5 3($2)
        addi $5 $0 0x0
        addi $6 $0 0x3
        jrl CHECK
LOOP:
        add $9 $2 $5
        load $7 -0($9)
        load $8 +1($9)
        add $7 $7 $8
        store $7 0($9)
        addi $5 $5 0x1
CHECK:
        cmp $6 $5
        bgt LOOP
