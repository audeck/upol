rect: rect.o
	ld -o rect rect.o

rect.o: rect.asm
	nasm -f elf64 rect.asm

mypwd: mypwd.o
	ld -o mypwd mypwd.o

mypwd.o: mypwd.asm
	nasm -f elf64 mypwd.asm

mypwd2: mypwd2.o
	ld -o mypwd2 mypwd2.o

mypwd2.o: mypwd2.asm
	nasm -f elf64 mypwd2.asm

tutorial07: tutorial07.o
	ld -o tutorial07 tutorial07.o

tutorial07.o: tutorial07.asm
	nasm -f elf64 tutorial07.asm