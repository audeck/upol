main: main.o cond.o
	gcc -o main main.o cond.o

main.o: main.c
	gcc -c main.c

cond.o: cond.asm
	nasm -f elf64 cond.asm