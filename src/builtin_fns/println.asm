_println:
    push	rbp
    mov	rbp, rsp
    
    mov	rsi, rdi
    mov	rdi, _println_fmt
    call    _printf
    
    pop	rbp
    ret

