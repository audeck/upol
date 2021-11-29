/* 
   CTVRTY BONUSOVY UKOL ZE ZP1

   Reseni odevzdejte do 6. 12. 2021 13:59 na email: petr.osicka@upol.cz 
   s predmetem Bonusovy ukol 4, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.
  

   ZADANI UKOLU:

   Naprogramujte funkci. 

   int decimate(char number[], int base)

   ktera prevede cislo number, ktere je zadano jako retezec, o zakladu
   base, na cislo v desitkove soustave a vrati jej jako navratovou hodnotu.
   Argument base muze byt v rozsahu 2 - 16, pricemz cislice v cislech
   o zakladu vetsim nez 10 reprezentujeme velkymi cisly.

   Muzeme predpoklad pouze cisla, ktera se vejdou do typu
   int.
   
   Priklad:
   int x = decimate("A1B", 12);

   v promenne x je ted 1463.
   
 */


#include <stdio.h>

/* Exponentiates 'base' to the power of 'exponent' and returns the result */
int power(int base, int exponent) {
    int result = 1;
    for (int i = 0; i < exponent; i++) result *= base;
    return result;
}

/* Returns the length of a string */
int stringLength(char string[]) {
    int i = 0;
    for (i = 0; string[i]; i++);
    return i;
}

/* Converts 'number' from base 'base' to decimal and returns the result */
int decimate(char number[], int base) {
    int result = 0;
    int number_length = stringLength(number);

    for (int i = 0; number[i]; i++) {
        if (48 <= number[i] && number[i] <= 57) {
            result += (number[i] - 48) * power(base, number_length - (i + 1));
        } else if (65 <= number[i] && number[i] <= 90) {
            result += (number[i] - 55) * power(base, number_length - (i + 1));
        } else if (97 <= number[i] && number[i] <= 122) {
            result += (number[i] - 87) * power(base, number_length - (i + 1));
        } else {
            return 0;  // Throw exception?
        }
    }

    return result;
}

int main(void) {
    // printf("%i\n", decimate("10", 10));         // 10
    // printf("%i\n", decimate("10", 2));          // 2
    // printf("%i\n", decimate("FF", 16));         // 255
    // printf("%i\n", decimate("100110101", 2));   // 309
    // printf("%i\n", decimate("A1B", 12));        // 1463
    // printf("%i\n", decimate("12345", 8));       // 5349

    return 0;
}