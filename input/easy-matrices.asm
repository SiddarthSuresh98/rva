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

        addi $7 $0 0x0          ; tracks M
        jrl  MCOND

MITER:
        addi $8 $0 0x0          ; tracks P
        jrl  PCOND
PITER:
        addi $9 $0 0x0          ; tracks N
        addi $10 $0 0x0         ; tracks accumulated result

        load $6 n($0)           ; index m1
        mul  $11 $6 $7
        addi $11 $11 m1

        addi $12 $8 m2          ; index m2

        jrl NCOND
NITER:
        add  $13 $11 $9         ; increment m1

        load $6 p($0)
        mul  $14 $9 $6          ; increment m2
        add  $14 $14 $12

        load $6 0($13)
        load $15 0($14)
        mul  $6 $6 $15
        add  $10 $10 $6

        addi $9 $9 0x1          ; increment element being added
NCOND:
        load $6 n($0)
        cmp  $6 $9
        bgt  NITER

        load $6 p($0)           ; store result
        mul  $6 $6 $7
        add  $6 $6 $8
        store $10 r($6)

        addi $8 $8 0x1          ; increment columns in m2
PCOND:
        load $6 p($0)
        cmp  $6 $8
        bgt  PITER

        addi $7 $7 0x1          ; increment rows in m1
MCOND:
        load $6 m($0)
        cmp  $6 $7
        bgt  MITER
        nop
        nop
        nop
        nop
        nop
        nop
