rm -f a.out
rm -f compiled.o

cargo run $1 -o compiled.asm -f $2

echo "generated asm:\n"
bat --tabs 8 compiled.asm
nasm -f $2 compiled.asm -o compiled.o
gcc -no-pie compiled.o -o program.out

rm -rf compiled*

