rm -f a.out
rm -f compiled.o

cargo run $1 > compiled.asm
ehco "generated asm:\n"
bat compiled.asm
nasm -f macho64 compiled.asm -o compiled.o
gcc compiled.o

rm -rf compiled*

