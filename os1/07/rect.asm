global _start

;;
;; Vytvořte program rect, který na terminál vypíše obdélník složený ze znaků '*' o stranách 20 x 5.
;;

;
; Constants
;
SYS_EXIT    equ 60    ; exit syscall number
EXIT_OK     equ 0     ; ok exit code

SYS_WRITE   equ 1     ; write syscall number
STDOUT      equ 1     ; STDOUT id

WIDTH     equ 20      ; width of the rectangle
HEIGHT    equ 5       ; height of the rectangle

;
; Data
;
section .data
    asterisks: times WIDTH db '*'
    newline: db 10
    asterisks_len: equ $-asterisks 
    ; NOTE: asterisks_len "includes" newline

;
; Executable code
;
section .text
_start:
    push rbx                ; store rbx on the stack
    mov rbx, HEIGHT         ; rbx: `HEIGHT` (loop variable)

write:
    mov rax, SYS_WRITE      ; write one line of asterisks (write syscall)
    mov rdi, STDOUT
    mov rsi, asterisks
    mov rdx, asterisks_len
    syscall

    dec rbx                 ; decrement rbx (= height)
    jnz write               ; loop if rbx != 0

    pop rbx                 ; restore rbx

    mov rax, SYS_EXIT       ; exit syscall
    mov rdi, EXIT_OK
    syscall
