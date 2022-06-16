
	section .data
_temp_29409:	dq 0
_temp_66643:	dq 0

	section .text

	global _sub
_sub:
	push	rbp

	mov	rax, rdi
	sub	rax, rsi
	pop	rbp
	ret

	global _add
_add:
	push	rbp

	mov	rax, rdi
	add	rax, rsi
	pop	rbp
	ret

	global _mul
_mul:
	push	rbp

	mov	rax, rdi
	mul	rsi
	pop	rbp
	ret

	global _main
_main:
	push	rbp

	mov	rdi, 4
	mov	rsi, 2
	call	_sub

	mov	[rel _temp_29409], rax
	mov	rdi, 3
	mov	rsi, [rel _temp_29409]
	call	_add

	mov	[rel _temp_66643], rax
	mov	rdi, 10
	mov	rsi, [rel _temp_66643]
	call	_printf

	pop	rbp
	mov	rax, 0
	ret
