# Shark Programming Language

**A Lisp-style toy programming language (WIP)**

#### How to use:
**right now Shark lacks a lot of the essential features of a programming language,**
**here's how to use this current (extremely primitive) version of Shark:**

Example code: add 2 numbers

```
(let a = 30)
(let b = 24)

(let c = (add a b))
(print_int c)
```

To compile a Shark program:

``` Bash
$ ./compile.sh program.shark
$ ./a.out
```

Note that the compile script will emit the compiled assembly code for debug

LICENSED UNDER GPLv3

