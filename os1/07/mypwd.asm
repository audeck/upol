global _start

;;
;; Vytvořte program mypwd, který se bude chovat podobně jako standardní 
;; unixový příkaz pwd a vypíše na standardní výstup plnou cestu k aktuálnímu
;; adresáři. Jaký je aktuální adresář zjistíte pomocí systémového volání getcwd.
;;

;
; Constants
;
SYS_EXIT    equ 60    ; exit syscall number
EXIT_OK     equ 0     ; ok exit code
EXIT_ERROR  equ 1     ; error exit code

SYS_WRITE   equ 1     ; write syscall number
STDOUT      equ 1     ; STDOUT id

SYS_GETCWD  equ 79    ; getcwd syscall number
BUFFER_SIZE equ 1024  ; size of cwd buffer

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
    mov rax, SYS_GETCWD          ; getcwd syscall
    mov rdi, buffer
    mov rsi, BUFFER_SIZE
    syscall

    cmp rax, 0                   ; check if getcwd succeeded
    jl error                     ; jump to error if not

    mov byte [buffer + rax], 10  ; add a newline character to buffer
    inc rax                      ; increment rax (= length of buffer)

    mov rdx, rax                 ; write r10 buffer to stdout (write syscall)
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, buffer
    syscall

    mov rax, SYS_EXIT            ; exit with ok code (exit syscall)
    mov edi, EXIT_OK
    syscall

error:
    mov rax, SYS_WRITE           ; write error message to stdout (write syscall)
    mov rdi, STDOUT
    mov rsi, error_msg
    mov rdx, error_len
    syscall

    mov rax, SYS_EXIT            ; exit with error code (exit syscall)
    mov rdi, EXIT_ERROR
    syscall
