global _start

;;
;; Vytvořte program mypwd2, který vypíše jméno aktuálního adresáře, tj. jméno za posledním znakem '/'.
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

    ; Loop setup
    mov r8, 0                    ; r8: loop buffer offset
    mov r9, 1                    ; r9: right-most '/' index + 1 (always a slash at buffer[0]);

loop_start:
    inc r8                       ; increment r8 to loop

    cmp byte [buffer + r8], 0    ; check if [buffer + r8] is the null-terminator
    je loop_end                 ; jump to loop_end if so

    cmp byte [buffer + r8], '/'  ; check if [buffer + r8] is a slash character
    jne loop_start               ; loop if not

    inc r8                       ; increment r8 to "skip over" the slash
    mov r9, r8                   ; update r9
    jmp loop_start               ; loop

loop_end:
    mov r10, buffer              ; move buffer to r10 to manipulate it
    add r10, r9                  ; offset r10 by r9
    sub rax, r9                  ; subtract r9 from rax (length of buffer (i.e. r10))

exit:
    mov rdx, rax                 ; write r10 buffer to stdout (write syscall)
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    mov rsi, r10
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
