main: main.o binary_heap/binary_heap.o
	gcc -o main main.o binary_heap/binary_heap.o

main.o: main.c
	gcc -c main.c

binary_heap.o: binary_heap/binary_heap.h binary_heap/binary_heap.c
	gcc -c binary_heap/binary_heap.c