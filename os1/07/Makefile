rect: rect.o
	ld -o rect rect.o

rect.o: rect.asm
	nasm -f elf64 rect.asm

mypwd: mypwd.o
	ld -o mypwd mypwd.o

mypwd.o: mypwd.asm
	nasm -f elf64 mypwd.asm