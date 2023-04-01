global _start

;
; Constants
;
SYS_EXIT    equ 60
EXIT_OK     equ 0
EXIT_ERROR  equ 1

SYS_WRITE   equ 1
STDOUT      equ 1

SYS_GETCWD  equ 79
BUFFER_SIZE equ 256

;
; Uninitialized data
;
section .bss
    buffer: resb BUFFER_SIZE

;
; Initialized data
;
section .data
    error_msg: db "Error: could not get working directory", 10
    error_len: equ $-error_msg

;
; Executable code
;
section .text
_start:
    ; Call getcwd
    mov rax, SYS_GETCWD
    mov rdi, buffer
    mov rsi, BUFFER_SIZE
    syscall

    ; Check if getcwd succeeded
    cmp rax, -1
    je error

    ; Add newline character to buffer
    mov byte [buffer + rax], 10
    inc rax

    ; Write buffer to stdout
    mov rdx, rax
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, buffer
    syscall

    ; Exit with ok code
    mov rax, SYS_EXIT
    mov edi, EXIT_OK
    syscall

error:
    ; Write error message to stdout
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, error_msg
    mov rdx, error_len
    syscall

    ; Exit with error code
    mov rax, SYS_EXIT
    mov rdi, EXIT_ERROR
    syscall
