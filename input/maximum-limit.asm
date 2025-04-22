;;;;;;;;
;;; tests integer boundaries and overflow condition

.data
.text
MAIN:
	addi  $5 $0 1
	sftli $5 $5 31
	addi  $6 $0 -1
	quot  $5 $5 $6
	bof   DONE
	addi  $5 $0 -1		; bad!
DONE:
	nop
	nop
	nop
