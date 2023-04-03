global _start

;
; deklarace konstant
;
SYS_READ        equ 0       ; systemove volani pro cteni ze souboru
SYS_WRITE       equ 1       ; systemove volani pro zapis do souboru
SYS_EXIT        equ 60      ; systemove volani pro ukonceni programu
STDIN           equ 0       ; deskriptor souboru standardniho vstupu
STDOUT          equ 1       ; deskriptor souboru standardniho vystupu
EOK             equ 0       ; konstanta signalizujici, ze program skoncil v poradku
EINPUT          equ -1      ; konstanta signalizujici, ze program skoncil chybou

INPUT_BUF_SIZE  equ 1       ; velikost vstupniho bufferu (1 byte)
COUNT_BUF_SIZE  equ 16      ; velikost bufferu pro pocet radku
NUMBER_BASE     equ 10      ; soustava poctu radku
C_NEWLINE       equ 10      ; hodnota znaku noveho radku
C_NULLTRM       equ 0       ; hodnota znaku ukonceni retezce

;
; buffery
;
section .bss
    input_buffer: resb INPUT_BUF_SIZE
    count_buffer: resb COUNT_BUF_SIZE

;
; spustitelny kod
;
section .text
_start:
    mov rcx, 0                 ; predpokladam, ze kazdy string je implicitne ukoncen "\n\0" (jako napr. echo)
    push rcx                   ; uloz rcx na zasobnik

echo_read:
    mov rax, SYS_READ          ; nacte data ze standardniho vstupu
    mov rdi, STDIN
    mov rsi, input_buffer
    mov rdx, INPUT_BUF_SIZE
    syscall

    cmp rax, 0
    je echo_end                ; pokud je vysledek 0 => konec echa
    jl fail                    ; pokud je vysledek zaporny => chyba

    cmp byte [input_buffer], C_NEWLINE
    jne echo_write             ; pokud znak neni '\n', preskoc

    pop rcx                    ; obnov rcx
    inc rcx                    ; rcx++
    push rcx                   ; uloz rcx

echo_write:
    mov rdx, rax               ; rax obsahuje pocet nactenych bytu (predavame jako 3. argument)
    mov rax, SYS_WRITE
    mov rdi, STDOUT            ; vypisujeme na standardni vystup
    mov rsi, input_buffer
    syscall                    ; vypsani obsahu bufferu

    jmp echo_read              ; echo loop

echo_end:                      ; konec echa echu (nemusi byt label)
    pop rcx                    ; obnov rcx
    push rcx                   ; uloz rcx na zasobnik
    push rbx                   ; uloz rbx -//-

    mov rax, rcx               ; uloz pocet radku (= rcx) v rax
    mov rbx, NUMBER_BASE       ; uloz deset (pro desitkovou soustavu) v rbx
    mov rdx, 0                 ; vynuluj rdx
    mov rsi, count_buffer      ; uloz adresu count_buffer v rsi

convert_count:                 ; prevede count (v rax) na byty znaku v count_buffer
    mov rdx, 0                 ; vynuluj rdx
    div rbx                    ; vydel rax deseti
    add dl, '0'                ; pridej ke zbytku hodnotu znaku 0
    mov byte [rsi], dl         ; pridej znak zbytku do rsi (= count bufferu)
    inc rsi                    ; posun rsi

    cmp rax, 0                 ; pokud rax neni 0 => loop
    jne convert_count

    pop rbx                    ; konec konvertovani => obnov rbx

    mov byte [rsi], C_NEWLINE  ; pridej newline char
    inc rsi

    mov byte [rsi], C_NULLTRM  ; pridej null terminator
    inc rsi

write_count:                   ; vypise pocet radku na radek stdout
    sub rsi, count_buffer      ; rozdil rsi - count_buffer = delka bufferu

    mov rdx, rsi               ; uloz delku do rdx
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, count_buffer
    syscall                    ; write syscall pro pocet radku

    jmp success

fail:                          ; chyba pri cteni dat
    mov rdi, EINPUT
    jmp exit

success:                       ; uspesne ukonceni programu
    pop rcx                    ; obnov rcx (= pocet radku)
    mov rdi, rcx               ; vrat rcx jako exit code

exit:                          ; predpoklada, ze v rdi je navratovy kod, a ukonci program
    mov rax, SYS_EXIT
    syscall
