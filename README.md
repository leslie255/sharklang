<img src="https://i.imgur.com/8y53ssF.png" alt="Shark logo" height="100px">

# The Shark Programming Language

<img src="https://i.imgur.com/VBBeGtY.png" alt="Version 0.0.1" height="24px"> <img src="https://i.imgur.com/Mae21iF.png" alt="Language Rust" height="24px">

## Showcase

Hello, world:

<img src="https://i.imgur.com/zAjh2qP.png" alt="Version 0.0.1" height="256px">

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

## Feature highlights
- Functions, variables, if statements (if, else if, else), loops
- Pointers
- Rich type expression (functions are also a type `(arg: T) -> T`)
- Type checker
- Inline assembly
- Macros
- Good error prompt
- ABI-compatible with C

## Known issues
- Incorrect behaviors with data types smaller than 64 bits

## Documentation

First of all, Shark currently does not have a standard library, everything is done with C's STD (since Shark and C are ABI-compatible), this includes the `main` function, and things like `printf`.

### Using the compiler
#### System requirements
The binary compiled by `sharkc` can only run on 64-bit x86 CPU's, and it is only tested on Linux and macOS.
`sharkc` itself might work on other systems, but it is not tested.

#### Dependencies
- `cargo` for building the `sharkc` binary
- `nasm` and `gcc` (or `clang`) for assembling and linking the assembly code output by `sharkc`

#### Install and use the compiler
To build the `sharkc` binary, first clone the repository, and then fetch the submodule `madeline` (a compiler backend I wrote):

```
git submodule update --init --recursive --remote
```

And then build the binary using cargo:

```
cargo build --release
```

The compiler would output assembly code, you can either manually assemble and link the assembly output or use the `compile.py` script.

To compile, assemble and link manually:
``` Bash
# if you are on a Mac, replace `elf64` with `macho64`
$ ./target/release/sharkc -f elf64 program.shark -o output.asm
$ nasm -f elf64 output.asm -o output.o
# for now you have to link with `-no-pie` on Linux
$ gcc -no-pie output.o

# run the program:
$ ./a.out
```

Using the `compile.py` script, first install the compiler:
- Create the directory `~/.config/sharkc/`
- Move `compile.py` and `target/release/sharkc` into `~/.config/sharkc/`
- Add `alias sharkc="python3 ~/.config/sharkc/compile.py"` to your shell config files

Using the installed compiler:
``` Bash
# If you are on a Mac, replace `elf64` with `macho64`
$ sharkc program.shark -f elf64 -o a.out

# run the program:
./a.out
```

### Functions and variables
In Shark, variables and functions are declared using the same syntax:

```
name: T = RHS
```

To declare a function, just declare a variable with function as its type `(arg: T) -> T`, and a block `{ ... }` as its RHS.

Note that function types without any arguments or no return values can be abbreviated as shown below:

```
// function with arguments and return values
sum: (x: usize, y: usize) -> usize = { ... }

// function with no return values
add: (x: *usize, y: usize) -> none = { ... }
add: (x: *usize, y: usize) = { ... }

// function with no arguments but a return value
main: () -> u32 = { ... }
main: -> u32 = { ... }

// function with no arguments or return value
print_message: () -> none = { ... }
print_message: () = { ... }
```

Having a trailing comma `,` in function argument expressions is also allowd:

```
main: (argc: u32, argv: **u8, ) = { ... }
```

Functions with variadic arguments can be annotated using `..`:

```
extern printf: (fmt: *u8, args: usize ..);
```

Note that the `extern` keyword means that the symbol is declared elsewhere in another file.

To declare variable, use a similar syntax, you can also leave the type expression blank for the compiler to infer a type

```
x: usize = 0;
y: = x;
```

### Data types
Basic type names in Shark is similar to those in Rust:
```
Unsigned integers:  u8, u16, u32, u64, usize,
Signed integers:    i8, i16, i32, i64, isize,
Floating point:     f32, f64,
```

Note that `Shark` has a type called `none` that works similar to the C `void`,

There are no dedicated boolean types, but there is `true` and `false`, which equals to `1` and `0`,

Pointers are annotated using `*` before the type, such as `*none`, and `**u8`,

You can also define a custom type using `typedef`:

```
typedef str = *u8;
```

Structure and slice types are currently able to be parsed, but they can't be compiled.

```
// structure:
typedef Person = {
    name: [u8],     // slice
    age: usize,
    gender: u8,
}
```

### Implicit `printf`
When encountering a string literal with no further context, the compiler implicitly calls the `printf` function:
```
"hello, world\n";   // this just prints "hello, world"
```

### If statements and loops
If statements works as you would expect:
```
if a < 100 {
    "a is small\n";
} else if a > 100 {
    "ONE\n";
} else {
    "A HUNDRED!\n";
}
```

Comparation operators - `==`, `!=`, `>`, `<`, `>=`, `<=` currently only works inside `if` conditions.

There are no `for` or `while` loops yet, but there is a `loop` keyword that does an infinite loop, and you would need to manually break out of it:

`break` and `continue` works normally.

```
i: = 0;
loop {
    printf("%lu\n", i);
    if i == 10 {
        break;
    }
}
```

### `#import` and `#include`
`#include file` works similarly as `#include "file"` in C. It just inserts the content of `file` into the file you're using the `#include` statement in.

`#import file` is like `#include <file>` in C, it searches in a defined import dir for the file you're trying to include. The default import dir in Shark is at `~/.config/sharkc/import/`.

### Using `math.hhh`
Because math expressions haven't been implemented yet, you'll need to use the functions in `math.hhh` to do arithmatic operations. Take the `add` function as an example, you can either use the `add(&a, b)` function to perform `a += b`, or use the `sum(a, b)` function to return the result of `a + b`.

The complete list of math functions are listed below:
```
add(*usize, usize)
sub(*usize, usize)
mul(*usize, usize)
div(*usize, usize)

sum(usize, usize)
diff(usize, usize)
prod(usize, usize)
quot(usize, usize)

iadd(*isize, isize)
isub(*isize, isize)
imul(*isize, isize)
idiv(*isize, isize)

isum(isize, isize)
idiff(isize, isize)
iprod(isize, isize)
iquot(isize, isize)
```

### Macros and inline assembly

You can define a single-line macro or a multi-line macro like this:

```
// single-line macro:
#macro ZERO 0

// multi-line macro:
#macro SAY_HELLO
    printf("hello, world\n");
    printf("haha\n");
#end
```

The logic is that if there is at least one `\n` character between the identifier after `#macro` and the first non-whitespace character after it, it's a multi-line macro and it ends by the #end symbol, otherwise it is a single-line macro and it ends at `\n`.

To use a macro you need to use `#MACRO_NAME`, this is to make sure that you will know which identifiers or functions are a macro and which ones are just identifiers and functions. For defining a constant, there will be an `alias` feature in future versions, which will also allow function overloading.

Macros with parameters haven't been implemented yet.

Inline assembly can be used by the `#asm` keyword, like macros, inline assembly can also be multi-line.

```
// single-line inline assembly:
#asm mov    rax, 42

// multi-line inline assembly:
#asm
    mov     rax, rdi
    mul     rax, rsi
#end
```

Note that inline assembly have to be in x86 NASM.

When using inline assembly, you sometimes want to skip the return check because the returning is done in the assembly code. In this case, use `return _;` for unsafe return.

## Version 0.2 planned features
- Pointer assignment
- `for`/`while` loop
- Pins
- Local functions
- Allocate arrays on stacks
- Enums
- Macros with parameters
- Alias
- Slices
- Structures
- Math expressions

## Contributing
Issues and PR's are welcomed

## LICENSE
This project is Licensed under **GPLv3**
