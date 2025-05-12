;;;;;;;;
;;; adds adjacent elements of a 4-element vector together,
;;; storing the result in place. Uses the vector type.

.data
        arr 1 2 3 4
        s 3
.text
        load $4 s($0)   ; set the vector-length register
        addi $5 $0 arr
        srdl $16 $5 $16
        addi $6 $5 0x1
        srdl $17 $6 $16
        addv $18 $18 $17
        srds $18 $5 $15
        nop
        nop
        nop
