<img src="https://i.imgur.com/8y53ssF.png" alt="Shark logo" height="100px">

<h1>The Shark Programming Language</h1>

**A toy compiled programming language (WIP)**

<img src="https://imgur.com/VBBeGtY.png" alt="Version 0.0.1" height="24px"> <img src="https://imgur.com/Mae21iF.png" alt="Language Rust" height="24px">

### How to use:
Hello World:

``` Swift
// the main function is the entry point of the program
func main() -> int32 {
    // if encountered a string literal with no further context,
    // sharkc automatically knows that you want to print it
    "hello";
    // additionally, you can also use the `print` and `println` function to print a string
    print("wo");
    println("rld");

    // printing an integar requires the `u64print` function
    u64print(42);

    // like C the return value of `main` will be used as program exit code
    return 0;
}
```

To compile a Shark program:

``` Bash
# on macOS:
$ ./compile.sh program.shark macho64
# on Linux:
$ ./compile.sh program.shark elf64
$ ./program.out
```

`compile.sh` requires `nasm`, `gcc` and `bat`.

The compiled program can only run on x86_64 systems with (although the compiler itself *might* work on other systems), it has not been tested on Windows yet, so it probably won't work.

Declaring and assigning values to a variable:

``` Swift
// the following also have to be inside the main function,
// same with other examples below

// declaring a variable
let a: uint64 = 10;
let b: uint64 = a;
// assigning values to a variable
a = 20;	
let c: uint32 = 20;
b = c ~ uint64;	// use `~` for type cast
let c: uint64 = uadd(a, b);

u64print(c);

// strings variables are stored as pointer type `ptr`
let message: ptr = "hello";
println(message);
```

There are five builtin functions right now: `u64print`, `uadd`, `usub`, `umul`, `udiv`

``` Swift
// math functions
let a: uint64 = uadd(3, 4);
let b: uint64 = usub(a, 1);
let c: uint64 = umul(a, b);
let d: uint64 = udiv(c, usub(15, 12)); // Nested function calls are also supported

// printing integars using `print_int`
u64print(a);
u64print(b);
u64print(c);
u64print(d);
```

Defining a custom function:

``` Swift
func square(a: uint64) -> uint64 {
    return umul(a, a);
}

func main() -> int32 {
    "the square of 16 is...";
    let a: uint64 = square(16);
    u64print(a);

    return 0;
}
```

Use the `loop` keyword for loops

``` Swift
func main() -> int32 {
    let a: uint64 = 0;
    loop {
        a = uadd(a, 1);
        u64print(a);
    }
    
    return 0;
}
```

You can also write Assembly instructions directly among the Shark code:

(They have to be in x86_64 NASM)

``` Swift
func main() -> int32 {
    let a: uint64 = 2;

    // add 40 to `a`
    mov     rax, qword [rbp - 8]
    add     rax, 40
    mov     qword [rbp - 8], rax

    u64print(a); // will print out 42
	
    return 0;
}
```

Note that the break statement and if statement hasn't been implemented yet, so for now you can only use inline assembly to break out of a loop:
``` Swift
func main() -> int32 {
    let a: uint64 = 0;
    loop {
        a = uadd(a, 1);
        u64print(a);
        
        // if `a` equals 100, break out of the loop
        mov     rdi, 100
        mov     rax, [rbp - 8]
        cmp     rax, rdi
        je      _break_loop
    }
    _break_loop:
    
    return 0;
}
```

``` Swift
func unsafe_return_test() -> uint64 {
    mov	rax, 255
    _return;
}

func main() -> int32 {
    u64print(unsafe_return_test());
    return 0;
}
```

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

### LICENSE
This project is Licensed under **GPLv3**
