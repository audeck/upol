global _start

;
; Constants
;
SYS_EXIT  equ 60
EXIT_OK   equ 0

SYS_WRITE equ 1
STDOUT    equ 1

WIDTH     equ 20
HEIGHT    equ 5

;
; Data
;
section .data
    asterisks: times WIDTH db '*'
    newline: db 10
    asterisks_len: equ $-asterisks ; length including newline

;
; Executable code
;
section .text
_start:
    ; Store rbx to use it
    push rbx
    mov rbx, HEIGHT

write:
    ; Write one line of asterisks
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, asterisks
    mov rdx, asterisks_len
    syscall

    ; Repeat write `HEIGHT` times
    dec rbx
    jnz write

    ; Restore rbx
    pop rbx

    ; Exit
    mov rax, SYS_EXIT
    mov rdi, EXIT_OK
    syscall
