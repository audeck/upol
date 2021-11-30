/* 

   DRUHY DOMACI UKOL 


   Reseni odevzdejte do 17.12.2021 23:59 na email: petr.osicka@upol.cz 
   s předmětem Domaci ukol 2, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.

   ZADANI UKOLU.
   
   Naprogramujte funkci 

   void report (int p[], int n, int lb, int ub);

   p je pole celych cisel (kladnych i zapornych)
   n je velikost pole p, muzeme predpokladat, ze n je male cislo (napr. max 16)
   lb je dolni hranice souctu (soucet viz dale)
   rb je horni hranice souctu

   Funkce vypise vsechny neprazdne mnoziny indexu takove,
   ze pokud secteme hodnoty prvku v poli p na techto indexech, tak lb <= soucet <= rb 
   Kazdou mnozinu vypise program na zvlastni radek.

   
   Napriklad pokud
   p je pole {1,5,6,-1,1}, (n je pak 5), lb = 2, rb = 5, pak funkce vypise 
   
   {0, 4}   (protoze p[0] + p[4] = 2, a plati lb <= 2 <= rb)
   {1}      (protoze p[1] = 5, a plati lb <= 5 <= rb)
   {2, 3}   (protoze p[2] + p[3] = 5 a plati lb <= 5 <= rb) 
   
   a dalsi mnoziny.
   
   Ke generovani mnozin indexu pouzijte reprezentaci mnoziny pomoci 
   celeho cisla a bitove operatory (viz zacatek seminare a napoveda nize).
   Muzeme predpokladat, ze maximalni velikost pole se pocet bitu v typu int 
   zmenseny o 1.
   
   NAPOVEDY:

   Bitove operatory (priklady na 4 bitovych cislech)

   & ... bitova konjunkce,  1001 & 0101 = 0001
   | ... bitova disjunkce,  1001 | 0101 = 1101
   << ... bitovy posuv,     1001 << 1 = 0010, 1001 << 2 = 0100, 1001 << 3 = 1000

   test toho, jestli je i-ty bit v cisle a roven 1:
   a & (1 << (i-1))
   pokud je vysledek nenulovy, je i-ty bit roven 1

   dale je pro nas zajimava nasledujici tabulka (pro 3 bitova cisla), pro 
   cisla s vice bity je situace analogicka.

   cislo | binarne  | bity nastavene na 1
   --------------------------------------
     0   |   000    |    
     1   |   001    |    1
     2   |   010    |    2
     3   |   011    |    1,2
     4   |   100    |    3
     5   |   101    |    1,3
     6   |   110    |    2,3
     7   |   111    |    1,2,3
   ---------------------------------------

   vsechna cisla z pvniho sloupce jsou mensi nez 8 (1000);

 */


#include <stdio.h>

/* Expands to the binary digit of 'number' at 'index' (from the right) */
#define binDigit(number, index) ((number & (1 << index)) > 0)

/* Exponentiates 'base' to the power of 'exponent' and returns the result */
long int power(int base, int exponent) {
    long int result = 1;  // power(2, 16) technically does not fit in a 16-bit int
    for (int i = 0; i < exponent; i++) result *= base;
    return result;
}

/* Magically prints out array indexes depending on the binary representation of 'number' */
void magicalPrintMachine(int number, int bin_length) {
    int first = 1;

    for (int i = 0; i < bin_length; i++) {
        if (binDigit(number, bin_length - (i + 1))) {
            if (first) {
                printf("{%i", i);
                first = 0;
            } else {
                printf(", %i", i);
            }
        }
    }

    printf("}\n");
}

void report(int p[], int n, int lb, int rb) {
    for (unsigned int i = power(2, n) - 1; i > 0; i--) {
        int result = 0;

        for (int j = 0; j < n; j++) {
            result += p[n - (j + 1)] * binDigit(i, j);
        }

        if (lb <= result && result <= rb) {
            magicalPrintMachine(i, n);
        }
    }
}

int main(void) {
    // int p[] = {1, 5, 6, -1, 1};
    // report(p, 5, 2, 5);

    // {0, 1, 3}    (protoze p[0] + p[1] + p[3] = 5)
    // {0, 4}       (protoze p[0] + p[4] = 2)
    // {1, 3, 4}    (protoze p[1] + p[3] + p[4] = 5)
    // {1, 3}       (protoze p[1] + p[3] = 4)
    // {1}          (protoze p[1] = 5)
    // {2, 3}       (protoze p[2] + p[3] = 5)

    return 0;
}