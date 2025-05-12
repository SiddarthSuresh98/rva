;;;;;;;;
;;; demonstrates the push/pop functions by pushing and popping various values

.data
.text
MAIN:
        addi $5 $0 1
        push $5
        addi $5 $5 1
        push $5
        addi $5 $5 1
        push $5
        addi $5 $5 1
        push $5
        pop $5
        pop $6
        pop $7
        pop $8
        nop
        nop
