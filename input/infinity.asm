;;;;;;;;
;;; an infinite loop for testing simulator speed

.data
.text
        addi $5 $0 0x0
LOOP:
        addi $5 $5 0x1
        subi $5 $5 0x0
        jrl LOOP
        nop
        nop
        nop
