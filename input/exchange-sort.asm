;;;;;;;;
;;; implements an in-place exchange sort on arr.

.data
	arr -1318622599 378334259 1389637561 -1972785942 -445476377 -518496482 246156516 534976786 -559365316 208367589 745785340 -90469921 184774746 -1410662069 -1883632914 108684414 1923417598 195370117 -97708220 1094455633 -1708211084 1592917537 546295717 1629211316 184774746 1565966644 -1780827222 -108478896 257785146 -812141980 225278727 1658249937 179345962 -1252867426 714580745 747089361 -1247457813 -1169954607 646163740 271626267 724245185 -2015252233 2123295751 702108647 1068906112 586427974 339668453 698874317 -427386347 -1070106515 -951984659 1350515447 -511649125 -1864824724 -54104282 118450557 -98886723 1500703353 -26493647 1841550910
	s 60 			; array size

.text
	addi $5 $2 0x0		; establish frame pointer
	jmp MAIN


;;; accepts two arguments, corresponding to offsets into `arr'
;;; compares the corresponding values.
;;; if the first index is greater than the second, swaps the two values
;;; in place.
SWAPPER:
	push $5			;function preamble
	addi $5 $2 0x0
	subi $2 $2 0x0

	load $7 +2($5)		; access args
	load $6 +1($5)

	load $8 arr($7)		; index array
	load $9 arr($6)

	cmp $9 $8
	bgt EXIT

	store $9 arr($7)	; perform swap
	store $8 arr($6)

EXIT:
	addi $2 $5 0x0		;function postamble
	pop $5
	ret


MAIN:
	push $5			;function preamble
	addi $5 $2 0x0
	subi $2 $2 0x4

	load $6 s($0)		; inner loop termination condition
	subi $7 $6 0x1		; outer loop termination condition
	addi $8 $0 0x0		; outer loop iterator
	jrl OCON
O:
	addi $9 $8 0x1		; inner loop iterator
	jrl ICON
I:
	store $9 -1($5) 	; caller-saved registers
	store $8 -2($5)
	store $7 -3($5)
	store $6 -4($5)

	push $8
	push $9			; push args
	jal SWAPPER
	addi $2 $2 0x2		; cleanup args

	load $9 -1($5) 		; restore registers
	load $8 -2($5)
	load $7 -3($5)
	load $6 -4($5)
	addi $9 $9 0x1
ICON:
	cmp $6 $9
	bgt I
	addi $8 $8 0x1
OCON:
	cmp $7 $8
	bgt O
	nop
	nop
	nop
	nop
