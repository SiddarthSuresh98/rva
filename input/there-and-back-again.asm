;;;;;;;;
;;; tests subroutine calls using the JAL function

.data
.text
MAIN:
        jal PUSHER
        pop $5
        nop
        nop
        nop
        quot $0 $0 $0
PUSHER:
        push $2
        ret
        nop
        nop
        nop
        nop
