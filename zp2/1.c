#include <stdio.h>
#include <stdlib.h>

#define abs(a) ((a > 0) ? a : -a)
#define max(a, b) ((a > b) ? a : b)
#define is_in_range(a) ((a > 47) && (a < 58))

int string_length(char* string) {
    int i;
    for (i = 0; string[i]; i += 1);
    return i;
}

char* secti_cisla(char* num1, char* num2) {
    unsigned int index1 = string_length(num1);
    unsigned int index2 = string_length(num2);
    unsigned int index_res = max(index1, index2);
    printf("%i\n", index_res);
    int sign1 = (num1[0] == 45) ? -1 : 1;
    int sign2 = (num2[0] == 45) ? -1 : 1;
    int start1 = abs(sign1);  // 0 for positive numbers
    int start2 = abs(sign2);  // 1 for negative numbers (to accomodate their sign)
    int should_be_negative;
    int remainder = 0;
    int digit;

    /* Allocate result array */
    char* result = (char*) malloc(sizeof(char) * (index_res + 1));

    for (int i = 0; i < index_res + 1; i += 1) {
        result[i] = 64;
    }

    /* Decrement index "pointers" */
    index1--; index2--;

    /* abs(is_negative) == 0 for positive numbers, 1 for negative ones */
    while (index1 >= start1 && index2 >= start2) {
        if (is_in_range(num1[index1]) && is_in_range(num2[index2])) {
            printf("%c + %c = ", num1[index1], num2[index2]);
            digit = (sign1 * (num1[index1] - 48)) + (sign2 * (num2[index2] - 48)) + remainder;
            printf("%i\n", digit);
            should_be_negative = (digit < 0);  // (-0 == 0) :(
            remainder = (digit / 10);

            result[index_res] = 48 + abs(digit % 10);

            /* Decrement index "pointers" */
            index1--; index2--; index_res--;
        } else {
            free(result);
            return NULL;
        }
    }

    //result[index_res] = 0;

    for (int i = index1; i >= start1; i -= 1) {

    }

    return result;
}

int main(void) {
    char cislo1[] = "1212125476";
    char cislo2[] = "156";
    char* vysledek;

    vysledek = secti_cisla(cislo1, cislo2);
    printf("%s + %s = %s", cislo1, cislo2, vysledek);

    return 0;
}
