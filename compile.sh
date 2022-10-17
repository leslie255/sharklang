rm -f a.out
rm -f compiled.o

./target/release/sharkc $1 -o compiled.asm -f $2

echo "generated asm:\n"
bat --tabs 8 --theme=ansi compiled.asm
nasm -f $2 compiled.asm -o compiled.o
gcc compiled.o -o program.out

rm -rf compiled*

