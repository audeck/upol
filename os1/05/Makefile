main: main.o mem.o
	gcc -o main main.o mem.o

main.o: main.c
	gcc -c main.c

mem.o: mem.asm
	nasm -f elf64 mem.asm