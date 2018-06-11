.data
msg:
	.ascii    "Hello, world!\n"
	len = . - msg
.text
.global main
main:
	push %ebp
	mov %esp, %ebp
	push $msg
	call printf
	add $4, %esp
	mov %ebp, %esp
	pop %ebp
	ret
