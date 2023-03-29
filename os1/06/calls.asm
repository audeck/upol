global print_row
global print_rect
global factorial
global my_strdup
global fib
global print_facts

extern putchar
extern strlen
extern malloc
extern printi



section .text
;;
;; Vypíše na standardní výstup řádek skládající se z `n` opakování znaku `c`. 
;; Výpis by měl být ukončen znakem \n.
;;
;; void print_row(int n, char c);
;;
print_row:
    mov eax, edi          ; eax: `n`
    mov cl, sil           ; cl: `c`

print_row_loop:
    cmp eax, 0            ; pokud je `n` <= 0, skoc na _ret
    jle print_row_ret

    push rax              ; uloz promenne
    push rcx

    sub rsp, 32 + 8       ; WSL quirk(?) - segfaultuji na mem systemu bez teto instrukce

    mov dil, cl           ; uloz `c` do dil
    call putchar          ; zavolej putchar

    add rsp, 32 + 8       ; WSL quirk(?) - segfaultuji na mem systemu bez teto instrukce

    pop rcx               ; obnov promenne
    pop rax

    dec eax               ; sniz hodnotu `n` o 1
    jmp print_row_loop    ; opakuj

print_row_ret:
    mov dil, 0xA          ; uloz '\n' do dil
    call putchar          ; zavolej putchar
    ret



;;
;; Vykreslí na standardní výstup vyplněný obdélník skladájící se ze znaků '*',
;; mající rows řádků a cols sloupců.
;;
;; void print_rect(int rows, int cols);
;;
print_rect:
    mov eax, edi          ; eax: `rows`
    mov ecx, esi          ; ecx: `cols`

print_rect_loop:
    cmp eax, 0            ; pokud je `rows` == 0, skoc na _ret
    jle print_rect_ret

    push rax              ; uloz promenne
    push rcx

    mov edi, ecx          ; spravne zavolej print_row
    mov sil, '*'
    call print_row

    pop rcx               ; obnov promenne
    pop rax

    dec eax               ; sniz `rows` o 1
    jmp print_rect_loop   ; opakuj

print_rect_ret:
    ret



;;
;; Rekurzivním způsobem spočítá hodnotu faktoriálu čísla `n`.
;;
;; unsigned int factorial(unsigned int n);
;;
factorial:
    mov eax, edi         ; eax: `n`

    cmp eax, 1           ; pokud je `n` <= 1, skoc na _end
    jbe factorial_end

    push rax             ; uloz promennou

    dec edi              ; sniz edi o 1
    call factorial       ; zavolej factorial(`n` - 1)
    mov ecx, eax         ; uloz vysledek v ecx

    pop rax              ; obnov promennou
    mul ecx              ; vynasob `n` vysledkem factorial(`n` - 1)

    ret

factorial_end:
    mov eax, 1           ; uloz do eax 1 (pro `n` == 1 a `n` == 0)
    ret



;;
;; Vytvoří kopii řetezce `s`.
;;
;; char* my_strdup(char* s);
;;
my_strdup:
    mov r8, rdi         ; r8: `s` (pointer)
    push r8             ; uloz r8 

    call strlen         ; zavolej strlen
    mov rdi, rax        ; uloz vysledek strlen do rdi (argument)

    call malloc         ; zavolej malloc

    pop r8              ; obnov r8 (before malloc null check to clean up the stack)
    mov r9, rax         ; r9: pomocny pointer na alokovanou pamet

    test rax, rax       ; test rax
    jz my_strdup_ret    ; pokud rax == 0 (null), skoc na _ret


my_strdup_copy:         ; klasicka kopie strcpy (volani neni explicitne v zadani; pro jistou implementuji)
    mov cl, byte [r8]
    mov byte [r9], cl

    cmp cl, 0
    je my_strdup_ret

    inc r8
    inc r9
    jmp my_strdup_copy

my_strdup_ret:
    ret                 ; vrat rax



;;
;; Rekurzivně vypočítá hodnotu `n`-tého fibonacciho čísla.
;;
;; unsigned int fib(unsigned short n);
;;
fib:
    mov cx, di        ; cx: `n`
    cmp cx, 1         ; pokud je `n` <= 1, skoc na _end
    jbe fib_end

    ; --- fib(`n` - 1) ---

    dec cx            ; sniz cx o 1
    push rcx          ; uloz `n`

    mov di, cx        ; zavolej fib s `n` - 1
    call fib

    pop rcx           ; obnov `n`

    ; --- fib(`n` - 2) ---

    dec cx            ; sniz cx o 1
    push rax          ; uloz dosavadni vysledek

    mov di, cx        ; zavolej fib s `n` - 2
    call fib
    mov r8d, eax      ; uloz vysledek do r8d

    pop rax           ; obnov dosavadni vysledek
    add eax, r8d      ; pricti r8d k eax (dosavadni vysledek); fib(`n`) = fib(`n` - 1) + fib(`n` - 2)

    ret               ; vrat eax

fib_end:
    xor eax, eax      ; vynuluj cele eax
    mov ax, cx        ; uloz cx do ax (fib(1) = 1; fib(0) = 0)
    ret



;;
;; Vypíše prvních `n` hodnot faktoriálu s pomocí volání `printi` a `factorial`.
;;
;; void print_facts(unsigned char n);
;;
print_facts:
    mov cl, dil              ; cx: `n`

print_facts_loop:
    push rcx                 ; uloz cx

    xor edi, edi             ; vynuluj edi
    mov dil, cl              ; uloz cx do dil
    call factorial           ; zavolej factorial(`n`)

    mov edi, eax             ; uloz eax do (argumentu) edi
    call printi              ; zavolej printi

    pop rcx                  ; obnov cx
    
    cmp cl, 0                ; pokud je cx == 0, skoc na _ret
    je print_facts_ret

    dec cl                   ; sniz cx o 1
    jmp print_facts_loop     ; opakuj

print_facts_ret:
    ret