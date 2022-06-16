# Shark Programming Language

**A Lisp-style toy programming language (WIP)**

### How to use:
**right now Shark lacks a lot of the essential features of a programming language,**

**here's how to use this current (extremely primitive) version of Shark:**

Example code: simple math operations

```
(let a = (add 2 3))
(let b = (sub a 1))
(let c = (mul 3 b))
(let d = (div c b))

(print_int a)
(print_int b)
(print_int c)
(print_int d)
```

To compile a Shark program:

``` Bash
$ ./compile.sh program.shark
$ ./a.out
```

Note that the compile script will emit the compiled assembly code for debug

Also note that if you're on Linux you will have to change the `macho64` in `compile.sh` to `elf64`,

If you're on Windows, you will have to do `cargo run program.shark -o output.asm`, then assemble and link the generated assembly code using `nasm` and `gcc`

Some commits of the repo aren't usable (they will be marked), if the program doesn't work try the last few commits.
(I currently don't have a develop branch because it's now just me developing this myself)

### Contributing
Just open up a new PR and I'll review it as soon as possible. All issues and PR's should be in English.

LICENSED UNDER GPLv3

