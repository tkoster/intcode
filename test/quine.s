; vim: tabstop=8 shiftwidth=8 textwidth=80 noexpandtab syntax=intcode

.org 0

start:	rel #1
	out -1,R
	add 100 #1 100
	teq 100 #16 101
	bf 101 start
end:	hlt
