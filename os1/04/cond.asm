global sgn
global max2c
global min3us
global kladne
global mocnina
global flag_test
global oflag_test

section .text

;;
;; int sgn(int i);
;;
sgn:
    mov ecx, edi
    mov eax, 1

    cmp ecx, 0
    jge .non_negative
    dec eax

.non_negative:
    jg .positive
    dec eax

.positive:
    ret

;;
;; char max2c(char a, char b);
;;
max2c:
    mov al, dil
    mov bl, sil

    cmp al, bl
    jge .return
    mov al, bl

.return:
    ret

;;
;; unsigned short min3us(unsigned short a, unsigned short b, unsigned short c);
;;
min3us:
    mov ax, di

    cmp ax, si
    cmova ax, si
    cmp ax, dx
    cmova ax, dx
    ret

;;
;; int kladne(int a, int b, int c);
;;
kladne:
    mov eax, 0

    cmp edi, 0
    jle .return
    cmp esi, 0
    jle .return
    cmp edx, 0
    jle .return

    inc eax

.return:
    ret

;;
;; int mocnina(int n, unsigned int m);
;;
mocnina:
    mov eax, 1
    mov ecx, esi

.mocnina_loop:
    cmp ecx, 0
    jle .return
    imul edi
    sub ecx, 1
    jmp .mocnina_loop

.return:
    ret

;;
;; flag tests
;;
flag_test:
    mov al, dil
    mov bl, sil

    add al, bl  ; add al and bl (sets flags)
    mov al, 0   ; set al to 0 (preserves flags)
    
.check_zf:
    jnz .check_sf
    or al, 00000001b
.check_sf:
    jns .check_cf
    or al, 00000010b
.check_cf:
    jnc .check_of
    or al, 00000100b
.check_of:
    jno .return
    or al, 00001000b
.return:
    ret



oflag_test:
    mov al, 127
    mov bl, 1

    add al, bl  ; add al and bl (sets flags)
    mov al, 0   ; set al to 0 (preserves flags)
    
.check_zf:
    jnz .check_sf
    or al, 00000001b
.check_sf:
    jns .check_cf
    or al, 00000010b
.check_cf:
    jnc .check_of
    or al, 00000100b
.check_of:
    jno .return
    or al, 00001000b
.return:
    ret
