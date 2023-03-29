main: main.o calls.o
	gcc -no-pie -o main main.o calls.o

main.o: main.c
	gcc -c main.c

calls.o: calls.asm
	nasm -f elf64 calls.asm