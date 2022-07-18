_u64print:
	push    rbp
	mov	    rbp, rsp
	
	mov	    rsi, rdi
	mov	    rdi, _u64print_fmt
	call    _printf
	
	pop	    rbp
	ret

