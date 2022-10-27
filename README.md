<img src="https://i.imgur.com/8y53ssF.png" alt="Shark logo" height="100px">

<h1>The Shark Programming Language</h1>

**A toy compiled programming language (WIP)**

<img src="https://i.imgur.com/VBBeGtY.png" alt="Version 0.0.1" height="24px"> <img src="https://i.imgur.com/Mae21iF.png" alt="Language Rust" height="24px">

### Showcase

Hello, world:

<img src="https://i.imgur.com/zAjh2qP.png" alt="Version 0.0.1" width="30%">

```
#import cstd.hhh

main: -> u32 = {
    "hello, world\n";
    return 0;
}
```
(Imported files need to be in `~/.config/sharkc/import/`, you can find `cstd.hhh` in `test/`)

Variables and functions
```
#import cstd.hhh

cow_say: (str: *u8) = {
    printf("The cow says: %s\n", str);
}

main: -> u32 = {
    cow_say("moo");
    num: = 15;      // has type infer
    printf("num = %lu", num);
    return 0;
}
```

If statements and loops:
```
#import cstd.hhh
#import math.hhh

main: -> u32 = {
    // Fibonacci sequence
    a: = 1;
    b: = 1;
    temp: usize;
    i: = 0;
    loop {
        temp = a;
        // Math operators haven't been implemented yet,
        add(&a, b);
        b = temp;
        printf("%lu\n", a);

        inc(&i);
        if i == 20 {
            break;
        }
    }
    return 0;
}
```

Inline assembly:
```
#import cstd.hhh

square: (x: usize) -> usize = {

    #asm
        mov     rax, rdi
        mul     rdi
    #end

    return _;
}

main: -> u32 = {
    printf("The square of 4 is ... %lu\n", square(4));
    return 0;
}
```

### Current features
- Functions, variables, if statements (if, else if, else), loops
- Pointers
- Rich type expression (functions are also a type `(arg: T) -> T`)
- Type checker
- Inline assembly
- Macros
- Good error prompt
- ABI-compatible with C

### Documentation
> TODO

### Contributing
Issues and PR's are welcomed

### LICENSE
This project is Licensed under **GPLv3**
