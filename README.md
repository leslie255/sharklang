# Shark Programming Language

**My toy programming language (WIP)**

### How to use:
**right now Shark lacks a lot of the essential features of a programming language,**

**here's how to use this current (extremely primitive) version of Shark:**

Hello World:

``` Swift
// the main function is the entry point of the program
func main() {
    // if encountered a string literal with no further context,
    // sharkc automatically knows that you want to print it
    "hello, world";

    // printing an integar requires the `print_int` function
    print_int(42);

    // like C the return value of `main` will be used as program exit code
    return 0;
}
```

To compile a Shark program:

``` Bash
$ ./compile.sh program.shark
$ ./a.out
```

`compile.sh` will emit the assembly code generated for debug

You will need `nasm`, `gcc` and `bat` to compile (`gcc` won't be needed in the future when the Shark Standard Library is built).

Note that if you're on Linux you will have to change the `macho64` in `compile.sh` to `elf64`,
if you're on Windows, you will have to do `cargo run program.shark -o output.asm`, then assemble and link the generated assembly code using `nasm` and `gcc`.

Declaring and assigning values to a variable:

``` Swift
// the following also have to be inside the main function,
// same with other examples below

// declaring a variable
let a: uint64 = 10;
let b: uint64 = a;
// assigning values to a variable
a = 20;	
let c: uint64 = addint(a, b);

print_int(c);
```

There are five builtin functions right now: `print_int`, `addint`, `subint`, `mulint`, `divint`

``` Swift
// math functions
let a: uint64 = addint(3, 4);
let b: uint64 = subint(a, 1);
let c: uint64 = mulint(a, b);
let d: uint64 = divint(c, subint(15, 12)); // Nested function calls are also supported

// printing integars using `print_int`
print_int(a);
print_int(b);
print_int(c);
print_int(d);
```

You can also write Assembly instructions directly among the Shark code:

(They have to be in x86_64 NASM)

``` Swift
let a: uint64 = 2;

// add 40 to `a`
mov rax, qword [rbp - 8]
add rax, 40
mov qword [rbp - 8], rax

print_int(a);
```

These three lines of assembly added 40 to `a`, so the program will output `42` instaed of `2`

Defining a function:
``` Swift
func square(a) {
    return mulint(a, a);
}

func main() {
    "the square of 16 is...";
    let a: uint64 = square(16);
    print_int(a);

    return 0;
}
```

The compiled program can only run on 64-bit systems (although the compiler itself might work on 32-bit systems)

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

LICENSED UNDER GPLv3

### TODO
- 32, 16, 8 bit types
- floating point numbers and signed integars
- characters
- pointers
- loops
- if statement
- type infer
- standard library
