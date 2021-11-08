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

/* Return the length of a string */
int stringLength(char *string) {
    int i;
    for (i = 0; string[i]; i++);
    return i;
}

/* Deletes the character from a string at a given index (shifts all character after it to the left) */
void deleteChar(char *string, int index) {
    for (int i = index + 1; i <= stringLength(string); i++) {
        string[i - 1] = string[i];
   }
}


/* Deletes a word (all non-space chars at and before index) 
void deleteWord(char *string, int index) {
    /* Special case for end word (delete word and space before word) 
    if (string[index + 1] == 0) {
        int i;
        for (i = index; string[i] != 32 && i >= 0; i--) deleteChar(string, i);
        deleteChar(string, i);

    /* Otherwise delete word and space after the word 
    } else {
        deleteChar(string, index + 1);
        for (int i = index; string[i] != 32 && i >= 0; i--) deleteChar(string, i);
    }
}
*/

void deleteWord(char *string, char* word) {
    int word_length = stringLength(word);
    int deletable;

    for (int i = 0; i <= stringLength(string) - word_length; i++) {
        if (string[i + word_length] == 0 || string[i + word_length] == 32) {
            deletable = 1;

            for (int j = 0; j < word_length; j++) {
                printf("%c ==", string[i + j]);
                printf(" %c\n", word[j]);
                if (string[i + j] != word[j]) {
                    deletable = 0;
                    break;
                }
            }

            if (deletable) {
                for (int j = 0; j < word_length; j++) {
                    deleteChar(string, i);
                }

                // TODO: Delete space.
                if (string[i] == 32) {
                    deleteChar(string, i);
                }
            }
        }
    }
}

/* Deletes all words occurences from src TODO */
int deleteWords(char *src, char* words) {
    int deleted = 0;
    int deletable = 1;
    int src_offset = 0;
    int words_offset = 0;

    for (int i = 0; src[i]; i++) {
        for (int j = 0; words[j]; j++) {
           if (words[j] == 32) {
                if (src[i + (j - words_offset)]) {

                }
                words_offset = j;
                deletable = 1;
            } else {
                //if (words[j] == src[i + (j - words_offset)])
            }
        }
    }

    return deleted;
}

int main() {

    char s[] = "aa bb cc nejaka slova uprostred dd";
    char w[] = "bb cc dd ahoj priklad slov";

    printf("%i\n", 32 == s[2] ? 1 : 0);

    deleteWord(s, "nejaka");
    deleteWord(s, "dd");
    printf("%s END\n", s);

    //int pocet = deleteWords(s, w);

    //printf("smazano: %i\nnovy retez: %s", pocet, s);

  /* 
     predchozi printf vytiskne: 

     smazano: 3
     novy retez: aa nejaka slova uprostred  
   */
    
  
    return 0;
}
