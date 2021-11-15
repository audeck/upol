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
#include <stdlib.h>

/* Returns the length of a string (= the index of it's trailing 0) */
int stringLength(char string[]) {
    int i;
    for (i = 0; string[i]; i++);
    return i;
}

/* Memcpy workaround for strings */
void stringCopy(char copy_from[], char copy_to[]) {
    for (int i = 0; copy_from[i]; i++) {
        copy_to[i] = copy_from[i];
    }
}

/* Deletes the character from a string(!) at a given index (shifts all characters after it by one) */
void deleteChar(char string[], int index) {
    for (int i = index + 1; i <= stringLength(string); i++) {
        string[i - 1] = string[i];
   }
}

/* Normalizes whitespace, removes leading spaces, doubled spaces, and trailing spaces */
void stringFormat(char string[]) {
    /* Replace newline characters and tabs with spaces */
    for (int i = 0; string[i]; i++) {
        if (string[i] == '\n' || string[i] == 9) {
            string[i] = ' ';
        }
    }

    /* Remove leading spaces */
    while (string[0] == ' ') {
        deleteChar(string, 0);
    }

    /* Remove doubled (and trailing) spaces */
    for (int i = 0; i < stringLength(string) - 1; i++) {
        while (string[i] == ' ' && string[i + 1] == ' ') {
            deleteChar(string, i);
        }
    }

    /* Remove last possible trailing space */
    if (string[stringLength(string) - 1] == ' ') {
        deleteChar(string, stringLength(string) - 1);
    }
}

/* Prints a slice of a string */
void printStringSlice(char string[], int start, int end) {
    for (int i = start; i < end; i++) {
        printf("%c", string[i]);
    }
}

/* Prints out left-justified 'src' with a line length limit of 'line_len' */
void justify_left(char src[], int line_len) {
    int last_space = 0;  // Index of last space for line breaking
    int line_offset = 0;  // Index of current line's first character

    /* Copy original string as to not change it, and format the copied one */
    char *src_copy = malloc(stringLength(src) * sizeof(char));
    stringCopy(src, src_copy);  // No memcpy ;-;
    stringFormat(src_copy);
    int src_len = stringLength(src_copy);

    /* Go through 'src_copy' */
    for (int i = 0; i <= src_len; i++) {
        /* Print out previous line if over 'line_len' */
        if (i - line_offset > line_len) {
            printStringSlice(src_copy, line_offset, last_space);
            printf("\n");
            line_offset = last_space + 1;
        }

        /* Remember last whitespace/end of string */
        if (src_copy[i] == ' ' || src_copy[i] == 0) {
            last_space = i;
        }
    }

    /* Print out last line */
    printStringSlice(src_copy, line_offset, last_space);
    printf("\n");

    /* Release allocated memory */
    free(src_copy);
}

/* Prints out right-justified 'src' with a line length limit of 'line_len' */
void justify_right(char src[], int line_len) {
    int last_space = 0;  // Index of last space for line breaking
    int line_offset = 0;  // Index of current line's first character

    /* Copy original string as to not change it, and format the copied one */
    char *src_copy = malloc(stringLength(src) * sizeof(char));
    stringCopy(src, src_copy);  // No memcpy ;-;
    stringFormat(src_copy);
    int src_len = stringLength(src_copy);

    /* Go through 'src_copy' */
    for (int i = 0; i <= src_len; i++) {
        /* Print out line if over 'line_len' */
        if (i - line_offset > line_len) {
            for (int j = 0; j < line_len - (last_space - line_offset); j++) printf(" ");  // Print leading spaces
            printStringSlice(src_copy, line_offset, last_space);
            printf("\n");
            line_offset = last_space + 1;
        }

        /* Remember last whitespace/end of string */
        if (src_copy[i] == ' ' || src_copy[i] == 0) {
            last_space = i;
        }
    }

    /* Print out last line */
    for (int j = 0; j < line_len - (last_space - line_offset); j++) printf(" ");  // Print leading spaces
    printStringSlice(src_copy, line_offset, last_space);
    printf("\n");

    /* Release allocated memory */
    free(src_copy);
}

int main() {
    char test1_src[] = "Vzorovy text ukazuje\n priklad zarovnani vlevo, popripade vpravo";
    int test1_line_len = 30;

    justify_left(test1_src, test1_line_len);
    printf("%s", "\n");
    justify_right(test1_src, test1_line_len);

    return 0;
}
