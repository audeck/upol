global obsah_obdelnika
global obvod_ctverce
global obsah_ctverce
global obvod_trojuhelnika
global obvod_trojuhelnika2
global obsah_trojuhelnika2
global obsah_trojuhelnika3
global objem_krychle
global avg

section .text

;;
;; vypocet obsahu obdelnika
;; funkce ma 2 argumenty:
;;  - 2 strany obdelnika (a, b)
;;
obsah_obdelnika:
    mov eax, edi  ; ulozi stranu A do eax
    mul esi       ; vynasobi eax o stranu B

    ret

;;
;; vypocet obvodu ctverce
;; funkce ma 1 argument:
;;  - strana ctverce
;;
obvod_ctverce:
    mov eax, edi  ; ulozi stranu ctverce do eax
    add eax, eax  ; pricte stranu ctverce k eax
    add eax, eax  ; -//-

    ret

;;
;; vypocet obsahu ctverce
;; funkce ma 1 argument:
;;  - strana ctverce
;;
obsah_ctverce:
    mov eax, edi  ; ulozi stranu ctverce do eax
    mul eax       ; umocni eax na druhou

    ret

;;
;; vypocet obvodu obecneho trojuhelnika
;; funkce ma 3 argumenty:
;;  - 3 strany trojuhelnika (a, b, c)
;;
obvod_trojuhelnika:
    mov eax, edi  ; ulozi stranu A do eax
    add eax, esi  ; pricte stranu B k eax
    add eax, edx  ; pricte stranu C k eax

    ret

;;
;; vypocet obvodu rovnostranneho trojuhelnika
;; funkce ma 1 argument:
;;  - strana trojuhelnika
;;
obvod_trojuhelnika2:
    mov eax, edi  ; ulozi stranu trojuhelnika do eax
    add eax, edi  ; pricte stranu trojuhelnika k eax
    add eax, edi  ; -//-

    ret

;;
;; vypocet obsahu pravouhleho trojuhelnika
;; funkce ma 2 argumenty:
;;  - 2 prepony
;;
obsah_trojuhelnika2:
    mov eax, edi  ; ulozi jednu preponu do eax
    mul esi       ; vynasobi eax druhou preponou

    mov ecx, 2    ; ulozi 2 do ecx
    div ecx       ; vydeli eax ecx (dvouma)

    ret

;;
;; vypocet obsahu obecneho trojuhelnika
;; funkce ma 2 argumenty:
;;  - strana trojuhlenika
;;  - vyska odpovidajici teze strane
;;
obsah_trojuhelnika3:
    mov eax, edi  ; ulozi stranu trojuhelnika do eax
    mul esi       ; vynasobi eax vyskou

    mov ecx, 2    ; ulozi 2 do ecx
    div ecx       ; vydeli eax ecx (dvouma)

    ret

;;
;; vypocet objemu krychle
;; funkce ma 1 argument:
;;  - strana krychle
;;
objem_krychle:
    mov eax, edi  ; ulozi stranu krychle do eax
    mov ecx, edi  ; ulozi stranu krychle do ecx

    mul ecx       ; vynasobi eax ecx
    mul ecx       ; vynasobi eax ecx

    ret

;;
;; vypocet prumeru tri cisel
;; funkce ma 3 argumenty:
;;  - 3 cisla
;;
avg:
    mov eax, edi  ; ulozi prvni cislo do eax
    add eax, esi  ; prida k eax druhe cislo
    add eax, edx  ; prida k eax treti cislo

    mov ebx, 3    ; ulozi 3 do ecx
    xor edx, edx  ; vynuluje edx (jelikoz div -> edx:eax)
    div ebx       ; vydeli eax ecx (tremi)

    ret