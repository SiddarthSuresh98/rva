        addi $fp $0 0x200
        addi $5 $0 0x1
        store $5 0($fp)
        addi $5 $0 0x2
        store $5 1($fp)
        addi $5 $0 0x3
        store $5 2($fp)
        addi $5 $0 0x4
        store $5 3($fp)
        addi $5 $0 0x0
        addi $6 $0 0x3
        jrl CHECK
LOOP:
        add $9 $fp $5
        load $7 -0($9)
        load $8 +1($9)
        add $7 $7 $8
        store $7 0($9)
        addi $5 $5 0x1
CHECK:
        cmp $6 $5
        bgt LOOP
