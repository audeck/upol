main: main.o geometry.o
	gcc -o main main.o geometry.o

main.o: main.c
	gcc -c main.c

geometry.o: geometry.asm
	nasm -f elf64 geometry.asm