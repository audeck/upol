main: main.o dates.o bits.o circ.o
	gcc -o main main.o dates.o bits.o circ.o

main.o: main.c
	gcc -c main.c

dates.o: dates.h dates.c
	gcc -c dates.c

bits.o: bits.h bits.c
	gcc -c bits.c

circ.o: circ.asm
	nasm -f elf64 circ.asm