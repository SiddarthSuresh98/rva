;;;;;;;;
;;; adds adjacent elements of a 4-element vector together,
;;; storing the result in place.

.data
        arr 1 2 3 4
        s   3
        i   0

.text
        load $5 s($0)
        addi $10 $0 arr
        load $6 i($0)
        jrl CMP
L:
        add $9 $10 $6
        load $7 0($9)
        load $8 1($9)
        add $7 $7 $8

        store $7 0($9)
        addi $6 $6 0x1
CMP:
        cmp $5 $6
        bgt L
