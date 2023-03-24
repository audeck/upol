global swap
global division
global countdown
global nasobky
global minimum
global my_strlen
global my_strcat

section .text

;;
;; Prohodi hodnoty, ktere jsou dany ukazately a, b.
;;
;; void swap(int *a, int *b);
;;
swap:
    mov dword eax, [rdi]
    mov dword edx, [rsi]
    mov dword [rdi], edx
    mov dword [rsi], eax
    ret



;;
;; Vydeli x / y a ulozi vysledek do result, zbytek do remainder.
;;
;; void division(unsigned int x, unsigned int y, unsigned int *result, unsigned int *remainder)
;;
division:
    mov eax, edi
    mov r8, rdx
    mov r9, rcx

    xor edx, edx
    div esi

    mov dword [r8], eax
    mov dword [r9], edx
    ret



;;
;; Do daného pole values uloží posloupnost 10, 9, 8, ..., 1 (v tomto pořadí).
;;
;; void countdown(int *values);
;;
countdown:
    mov eax, 10
    mov ecx, 0
countdown_loop:
    cmp eax, 0
    je countdown_ret
    mov [rdi + rcx * 4], eax
    dec eax
    inc ecx
    jmp countdown_loop
countdown_ret:
    ret



;;
;; Do pole multiples uloží prvních deset násobků čísla n.
;;
;; void nasobky(short *multiples, short n);
;;
nasobky:
    mov rax, 0
    mov cx, si
nasobky_loop:
    cmp rax, 10
    je nasobky_ret
    mov [rdi + rax * 2], cx
    add cx, si
    inc rax
    jmp nasobky_loop
nasobky_ret:
    ret



;;
;; Vrací nejmenší prvek pole values obsahující count hodnot
;; nebo 2^31 - 1 pokud je count nula.
;;
;; int minimum(int count, int *values);
;;
minimum:
    mov eax, 0x7FFFFFFF
    xor rcx, rcx
    mov ecx, -1
minimum_loop:
    inc ecx
    cmp ecx, edi
    je minimum_ret
    cmp [rsi + rcx * 4], eax
    jge minimum_loop
    mov eax, [rsi + rcx * 4]
    jmp minimum_loop
minimum_ret:
    ret



;;
;; Stejne jako strlen.
;;
;; unsigned int my_strlen(char *s);
;;
my_strlen:
    mov eax, 0
my_strlen_loop:
    cmp byte [rdi], 0
    je my_strlen_ret
    inc eax
    inc rdi
    jmp my_strlen_loop
my_strlen_ret:
    ret



;;
;; Stejne jako strcar.
;;
;; void my_strcat(char *dest, char *src);
;;
my_strcat:
    test rsi, rsi
    je return
    test rdi, rdi
    je copy

    skip:
        cmp byte [rdi], 0
        je copy
        inc rdi
        jmp skip

    copy:
        mov cl, byte [rsi]
        mov byte [rdi], cl
        cmp cl, 0
        je return
        inc rdi
        inc rsi
        jmp copy

    return:
        ret
