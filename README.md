# Shark Programming Language

**My toy programming language (WIP)**

### How to use:
**right now Shark lacks a lot of the essential features of a programming language,**

**here's how to use this current (extremely primitive) version of Shark:**

Printing an integar:

``` Rust
// everything has to be inside the main function
func main() {

	print_int(42);

	return 0; // like C the return value of `main` will be used as program exit code
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

``` Rust
// the following also have to be inside the main function,
// same with other examples below

// declaring a variable
let a = 10;
let b = a;
// assigning values to a variable
a = 20;	
let c = addint(a, b);

print_int(c);
```

There are five builtin functions right now: `print_int`, `addint`, `subint`, `mulint`, `divint`

``` Rust
// math functions
let a = addint(3, 4);
let b = subint(a, 1);
let c = mulint(a, b);
let d = divint(c, subint(15, 12)); // Nested function calls are also supported

// printing integars using `print_int`
print_int(a);
print_int(b);
print_int(c);
print_int(d);
```

You can also write Assembly instructions directly among the Shark code:

(They have to be in x86_64 NASM)

``` Rust
let a = 2;

// add 40 to `a`
mov rax, [rel _a]
add rax, 40
mov [rel _a], rax

print_int(a);
```

These three lines of assembly added 40 to `a`, so the program will output `42` instaed of `2`

Defining a function:
``` Rust
func a_number() {
	return mulint(16, 16);
}

func main() {
	let a = subint(a_number(), 1);
	print_int(a);
	return 0;
}
```

Note that you currently cannot use variables of the same name across two different functions, this is because current all variables are statically defined in the data section, it will change in the future

The compiled program can only run on 64-bit systems (although the compiler itself might work on 32-bit systems)

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

LICENSED UNDER GPLv3

### TODO
- local variables (currently all variables are stored statically in the data section of the program)
- argument names
- types
- argument checking
- loops
- if statement
- type checking
- standard library

