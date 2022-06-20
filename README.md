# Shark Programming Language

**My toy programming language (WIP)**

### How to use:
**right now Shark lacks a lot of the essential features of a programming language,**

**here's how to use this current (extremely primitive) version of Shark:**

Printing an integar:

``` Rust
print_int(42);
```

To compile a Shark program:

``` Bash
$ ./compile.sh program.shark
$ ./a.out
```

Declaring and assigning values to a variable:

``` Rust
let a = 10;
let b = a;
let c = addint(a, b);
```

There are four builtin functions right now: `print_int`, `addint`, `subint`, `mulint`, `divint`

``` Rust
let a = addint(3, 4);
let b = subint(a, 1);
let c = mulint(a, b);
let d = divint(c, 2);

print_int(a);
print_int(b);
print_int(c);
print_int(d);
```

You can also write Assembly instructions directly among the Shark code:

(They have to be in x86_64 NASM)

``` Rust
let a = 2;

mov rax, [rel _a]
add rax, 40
mov [rel _a], rax

print_int(a);
```

These three lines of assembly basically added 40 to a, so the program will output `42` instaed of `2`

*Nested function calls (e.g. `print_int(addint(2, 3))`) are not supported yet*

Note that if you're on Linux you will have to change the `macho64` in `compile.sh` to `elf64`,

If you're on Windows, you will have to do `cargo run program.shark -o output.asm`, then assemble and link the generated assembly code using `nasm` and `gcc`.

Also note that the `compile.sh` will emit the compiled assembly code for debug.

The compiled program can only run on 64-bit systems (although the compiler itself might work on 32-bit systems)

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

LICENSED UNDER GPLv3

