/* 

   PRVNI DOMACI UKOL 
  
   
   Reseni odevzdejte do 21.11.2021 23:59 na email: petr.osicka@upol.cz 
   s předmětem Domaci ukol 1, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.
   
   ZADANI UKOLU: 
   

    Naprogramujte funkci s hlavickou

    int delete_words(char src[], char words[]);

    Oba argumenty funkce jsou retezce, ktere obsahuji slova oddelena
    mezerami (= mezi dvěma slovy je vždy jedna mezera).  Prvnim znakem
    neni mezera, za poslednim slovem neni mezera.  Za slovo povazujeme
    posloupnost znaku neprerusenych mezerou. Zadne dalsi bile znaky
    (napr. tabulator, novy radek apod.) se v retezci nevyskytuji.

    Priklad"
    - "toto je priklad spravneho vstupu" je priklad spravneho vstupu.
    - "  toto    neni    priklad \n spravneho   vstupu  " neni priklad spravneho vstupu.

    Funkce smaze z retezce src vsechna slova, ktera se nachazeji v
    retezci words. Pozadujeme, aby retezec src pote byl ve spravnem
    tvaru. Tedy prvnim znakem nesmi byt mezera, jednotliva slova jsou
    oddelena jednou mezerou, za poslednim slovem neni mezera.
    
    Funkce vrati jako navratovou hodnotu pocet smazanych slov.
    
    Je povolen pouze hlavickovy soubor stdio.h (specialne neni povolen string.h).

    NAPOVEDA:

    -> znak v retezci smazeme tak, ze vsechny znaky nalevo od nej (a nulu na
       konci retezce) posuneme o jednu pozici vlevo.

    -> ukol pred odevzdanim poradne otestujte, musi se chovat dobre i v situacich,
       kdy je jeden (nebo oba) argument prazdny retezec.

    -> pri pristupu pomoci indexu si davejte pozor na to, abyste nepristupovali
       mimo pole. To muzete testovat pomoci assert. 
    
    -> ukazka nize, neni jediny test, ktery budu na kodu zkouset.
   
*/


#include <stdio.h>

/* Returns the length of a string(!) */
int stringLength(char string[]) {
    int i;
    for (i = 0; string[i]; i++);
    return i;
}

/* Deletes the character from a string(!) at a given index (shifts all characters after it by one) */
void deleteChar(char string[], int index) {
    for (int i = index + 1; i <= stringLength(string); i++) {
        string[i - 1] = string[i];
   }
}

/* Removes leading spaces, doubled spaces, and trailing spaces */
void formatString(char string[]) {
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

    /* Remove the last possible trailing space */
    if (string[stringLength(string) - 1] == ' ') {
        deleteChar(string, stringLength(string) - 1);
    }
}

/* Deletes all 'words' word (whole string) occurences from 'src' (spaces function as delimiters between words) */
int deleteWords(char src[], char words[]) {
    /* Format 'src' and 'words' just in case it was passed in broken */
    formatString(src);
    formatString(words);
    int word_length;         // Length of current word
    int word_offset = 0;     // Index of the first character of the current word
    int deletable;           // Char deletion prerequisite
    int deletion_count = 0;  // Amount of words deleted (RETURN VALUE)

    /* Go through 'words' and find current word's length */
    for (int i = 0; i < stringLength(words); i++) {
        if (words[i + 1] == ' ' || words[i + 1] == 0) {
            word_length = i + 1 - word_offset;

            /* Go through 'src' */
            for (int j = 0; j <= stringLength(src) - word_length; j++) {
                /* See if word length lines up with the spaces in or end of 'src' */
                if (src[j + word_length] == ' ' || src[j + word_length] == 0) {
                    deletable = 1;

                    /* Compare chars of 'src' and the current word */
                    for (int k = 0; k < word_length; k++) {
                        if (src[k + j] != words[k + word_offset]) {
                            deletable = 0;
                            break;
                        }
                    }

                    /* Delete a 'word_length' amount of chars from 'words' (delete the current word) */
                    if (deletable) {
                        for (int k = 0; k < word_length; k++) {
                            deleteChar(src, j);
                        }

                        deleteChar(src, j - (src[j + word_length] == 0));
                        deletion_count++;
                    }
                }
            }

            /* Update offset so it "points" to the first char of the next word (won't overreach) */
            word_offset = i + 2;
        }
    }
    
    return deletion_count;
}

/* Pipes snake case into lower camel case, as functions should be :^) */
int delete_words(char src[], char words[]) {
    return deleteWords(src, words);
}


int main() {
    char s[] = "aa bb cc nejaka slova uprostred dd";
    char w[] = "bb cc slov dd";

    int pocet = delete_words(s, w);
    printf("Smazano: %i\nNovy retez: %s", pocet, s);

    /*

        Predchozi printf vytiskne: 

        Smazano: 3
        Novy retez: aa nejaka slova uprostred

    */
  
    return 0;
}
