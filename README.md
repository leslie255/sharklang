<img src="https://i.imgur.com/8y53ssF.png" alt="Shark logo" height="100px">

<h1>The Shark Programming Language</h1>

**A toy compiled programming language (WIP)**

<img src="https://i.imgur.com/VBBeGtY.png" alt="Version 0.0.1" height="24px"> <img src="https://i.imgur.com/Mae21iF.png" alt="Language Rust" height="24px">

### Quick Showcase

<img src="https://i.imgur.com/tOG69jN.png" width="40%">

```
#include cstd.hhh
#macro GREETING_MESSAGE "hello, world\n"

// Multi-line macros
#macro SAY_HELLO
    // Recursive macro expansion
    printf(#GREETING_MESSAGE)
#end

main: () -> u32 = {
    #SAY_HELLO;

    a: = 10; // type infer (but no forward-looking)
    p: *usize = &a; // can also specify a type

    // Inline assembly
    #asm
        mov rax, [rbp - 8]
        mov rcx, 4
        mul rcx
        add rax, 2
    #end

    printf("a = %lu\n", *p);
    return 0;
}
```

### Current features
- Functions, variables
- Pointer dereference and take address of variables
- Rich type expression (functions are also a type `(arg: T) -> T`)
- Inline assembly
- Macros
- Type checker
- ABI-compatible with C

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

### LICENSE
This project is Licensed under **GPLv3**
