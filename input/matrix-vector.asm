;;;;;;;;
;;; performs a matrix multiply the matrices m1 and m2:
;;; 9 4 3   1 0 4 2 2      65  21  55  72  40
;;; 1 4 2   8 3 1 9 4      49  18  18  50  22
;;; 9 4 7   8 3 5 6 2   =  97  33  75  96  48
;;; 6 6 6                  102 36  60  102 48
;;; the result of which is placed into r
;;; does not use designated vector instructions

.data
        m1 9 4 3 1 4 2 9 4 7 6 6 6
        m2 1 0 4 2 2 8 3 1 9 4 8 3 5 6 2
        n 3                     ; # of columns of m1, rows of m2
        m 4                     ; # of rows of m1
        p 5                     ; # of columns of m2
        r 0                     ; a matrix of size n,p

.text
        addi $5 $2 0x0          ; establish frame pointer
        jmp  MAIN

MAIN:
        push $5                 ; function preamble
        addi $5 $2 0x0
        subi $2 $2 0x0

;;;
;;; setup vector stuff, rotate num, column stride
        load $4 n($0)
        addi $14 $4 0x0
        load $6 p($0)
        load $7 m($0)
        addi $15 $0 0x1
        addi $8 $0 0x0
        jrl STRCOND
STR:
        addv $17 $17 $16        ; set column stride
        addi $8 $8 0x1
STRCOND:
        cmp $6 $8
        bgt STR
;;; end vector setup
;;;

        addi $8 $0 0x0          ; tracks M
        jrl  MCOND

MITER:
        load $4 n($0)           ; set vector length to n
        addi $9 $0 0x0          ; tracks P
        jrl  PCOND
PITER:
        mul  $10 $4 $8          ; index m1
        addi $10 $10 m1

        addi $11 $9 m2          ; index m2

        srdl $18 $10 $16        ; load row
        srdl $19 $11 $17        ; load column

        mulv $18 $18 $19        ; mul elements

        addi $13 $0 0x1         ; tracks iterations
        jrl ROTCOND
ROT:
        rotv $18 $18 $15
        addi $13 $13 0x1
ROTCOND:
        addi $4 $0 0x1
        addv $20 $20 $18
        addi $4 $14 0x0
        cmp $4 $13
        bgt ROT

        mul $13 $6 $8           ; store the result
        add $13 $13 $9
        addi $13 $13 r
        srds $20 $13 $16

        addi $4 $0 0x0          ; clear the accumulator
        addv $20 $20 $20
        addi $4 $14 0x0

        addi $9 $9 0x1          ; increment columns in m2
PCOND:
        cmp  $6 $9
        bgt  PITER

        addi $8 $8 0x1          ; increment rows in m1
MCOND:
        cmp  $7 $8
        bgt  MITER
        nop
        nop
        nop
        nop
        nop
        nop
