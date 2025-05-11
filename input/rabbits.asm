;;;;;;;;
;;; multiplies numbers until an overflow occurs

.data
	rabbits 2
	rate 2

.text
	load $5 rabbits($0)
	load $6 rate($0)

BREED:
	mul $5 $5 $6
	store $5 rabbits($0)
	bof DONE
	jrl BREED
DONE:
	nop
	nop
	nop
	div $0 $0 $0
