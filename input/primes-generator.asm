;;;;;;;;
;;; discovers factors from 0-N through use of the trial division algorithm.
;;;
;;; I didn't write a square root function, so this is worse than algorithms
;;; you'll find online. That may be better. :)

.data
        N 5000
        primes 0
.text
        addi $5 $2 0x0          ; establish frame pointer
        addi $6 $0 0x0          ; establish return argument
        jmp MAIN

TRIAL:
        push $5                 ;function preamble
        addi $5 $2 0x0
        subi $2 $2 0x0

        load $7 +1($5)          ; N (argument)
        addi $8 $0 0x2          ; the initial divisor
        addi $6 $0 0x0          ; the return value

        jrl TCOND
TLOOP:
        rem $9 $7 $8            ; check if divisible
        cmp $9 $0
        beq END

        addi $8 $8 0x1
TCOND:
        cmp  $7 $8
        bgt TLOOP

        addi $6 $0 0x1          ; return 1 (signal parameter is prime)
END:
        addi $2 $5 0x0          ; function postamble
        pop $5
        ret

MAIN:
        push $5                 ; function preamble
        addi $5 $2 0x0
        subi $2 $2 0x3

        load $7 N($0)           ; N
        addi $8 $0 0x2          ; number to test
        addi $9 $0 0x0          ; index to store

GEN:
        store $9 -1($5)         ; caller-saved registers
        store $8 -2($5)
        store $7 -3($5)

        push $8                 ; push arg
        jal TRIAL
        addi $2 $2 0x1

        load $9 -1($5)          ; restore registers
        load $8 -2($5)
        load $7 -3($5)

        cmp $6 $0               ; check if return is 0
        beq COMPOSITE

        store $8 primes($9)     ; store prime
        addi $9 $9 0x1
COMPOSITE:
        addi $8 $8 0x1
        cmp  $7 $8
        bgt GEN
        nop
        nop
        nop
