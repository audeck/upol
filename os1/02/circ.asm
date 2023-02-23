global rectangle_circumference

section .text

rectangle_circumference:
    mov edi, 10
    mov esi, 17
    mov eax, edi
    add eax, esi
    add eax, eax
    ret