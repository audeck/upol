/* 
   TRETI BONUSOVY UKOL ZE ZP1

   Reseni odevzdejte do 22.11.2020 13:59 na email: petr.osicka@upol.cz 
   s predmetem Bonusovy ukol 3, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.
  

   ZADANI UKOLU:

   Naprogramujte nasledujici funkce (kazda je za jeden bod). 

   void justify_left (char src[], int line_len);
   void justify_right (char src[], int line_len);
   
   Parametr src je retezec. Parametr line_len urcuje delku jednoho
   tiskoveho radku jako pocet vytistenych znaku.

   Ukolem funkci je obsah src vytisknout na obrazovku tak, aby jeden radek nepresahoval
   delku line_len, ale soucasne byl co nejdelsi (tj. pridanim dalsiho slova uz bychom
   delku line_len presahli). Radky nelze lamat v polovine slova,
   za slovo povazujeme posloupnost znaku, ktera neni
   prerusena bilym znakem: mezera, novy radek, tabulator.

   Funkce muze tisknout mene nebo vice bilych znaku nez je obsazeno
   v retezci src, musi ovsem zachovat stejna slova (tj. nelze 
   nevytisknout alespon jeden bily znak mezi dvema slovy).

   Funkce justify_left provede tisk tak, ze vsechny radky budou
   zacinat v prvnim textovem sloupci, tj. text je zarovnan doleva.

   Funkce justify_right provede tisk tak, ze vsechny radky budou
   koncit v poslednim textovem sloupci, tj. text je zarovnan doprava.

   Priklad zarovnani vlevo a vpravo

   ------------------------------
   Vzorovy text ukazuje priklad
   zarovnani vlevo
   
   ------------------------------
     Vzorovy text ukazuje priklad
                 zarovnani vpravo       

   Muzete predpokladat, ze nejdelsi slovo obsazene v src je kratsi
   nebo stejne dlouhe jako line_len.

   Z knihovny string.h je povolena pouze funkce strlen.

 */


#include <stdio.h>

int min(int first, int second) {
    return first < second ? first : second;
}

int stringLenght(char string[]) {
    int i;
    for (i = 0; string[i]; i++);
    return i;
}

void printStringSlice(char string[], int start, int end) {
    for (int i = start; i < end; i++) {
        printf("%c", string[i]);
    }
}

void cloneString(char copy_from[], char copy_to[]) {
    for (int i = 0; copy_from[i]; i++) {
        copy_to[i] = copy_from[i];
    }
}

/* 
    'justify_left' and 'justify_right' disregard all original whitespace before and after linebreak.
    Also, since the functions should print as many words per line as possible, they don't respect
    user newline characters (as they probably would if they were used in a text editor), instead
    they treat them as normal whitespace.
*/

void justify_left(char src[], int line_len) {
    int offset = 0;
    int src_len = stringLenght(src);
    // char src_clone[];
    // printf("%s", src_clone);

    for (int i = 0; offset < src_len; i++) {
        for (int j = min(line_len, (src_len - offset)); j >= 0; j--) {
            if (src[offset + j] == ' ' || src[offset + j] == ' ' || src[offset + j] == 0) {
                /* Print one line (src slice) */
                printStringSlice(src, offset, offset + j);
                printf("\n");

                /* Update offset (and disregard whitespace) */
                offset += j;
                while (src[offset] == ' ') {
                    offset++;
                }

                break;
            }
        }
    }
}

void justify_left(char src[], int line_len) {
    int last_valid = 0;
    int offset = 0;
    int src_len = stringLenght(src);

    for (int i = 0; i <= src_len; i++) {
        if (i - offset > line_len) {
            printStringSlice(src, offset, last_valid);
            printf("\n");

            offset = last_valid;
        }
    }
}

void justify_right(char src[], int line_len) {
    int offset = 0;
    int src_len = stringLenght(src);

    for (int i = 0; offset < src_len; i++) {
        for (int j = min(line_len, (src_len - offset)); j >= 0; j--) {
            if (src[offset + j] == ' ' || src[offset + j] == ' ' || src[offset + j] == 0) {
                /* Print leading whitespace */
                for (int k = 0; k < line_len - j; k++) {
                    printf("%s", " ");
                }

                /* Print one line (src slice) */
                printStringSlice(src, offset, offset + j);
                printf("\n");

                /* Update offset (and disregard all whitespace @ linebreak) */
                offset += j;
                while (src[offset] == ' ') {
                    offset++;
                }

                break;
            }
        }
    }
}

int main() {
    char test1_src[] = "Vzorovy text ukazuje\n priklad zarovnani vlevo, popripade vpravo";
    int test1_line_len = 30;

    justify_left(test1_src, test1_line_len);
    printf("%s", "\n");
    justify_right(test1_src, test1_line_len);

    return 1;
}