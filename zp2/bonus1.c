#include <stdio.h>
#include <stdlib.h>

/* 
    Assigns the amount of 'podretezec' occurences in 'text' to memory at 
    'pocet_vyskytu' and returns a pointer to an array of their indices. 
    Case insensitivity is guaranteed only for base ASCII characters (!).
*/
int* najdi(char* text, char* podretezec, int* pocet_vyskytu) {
    *pocet_vyskytu = 0;
    int* indexes = NULL;
    int identical;

    for (int i = 0; text[i]; i += 1) {
        identical = 1;

        for (int j = 0; podretezec[j]; j += 1) {
            /* 
                Convert lower to upper case ASCII - probably breaks non-ASCII
                comparison, but I am NOT straying away from 1-byte characters.
            */
            if (text[i + j] - 32 * (text[i + j] > 96 && text[i + j] < 123) != podretezec[j] - 32 * (podretezec[j] > 96 && podretezec[j] < 123)) {
                identical = 0;
                break;
            }
        }

        if (identical) {
            *pocet_vyskytu += 1;
            indexes = (int*) realloc(indexes, *pocet_vyskytu * sizeof(int));
            indexes[*pocet_vyskytu - 1] = i;
        }
    }

    return indexes;
}

int main(void) {
    int* occurences;
    char* text = "Máma mele maso~";  // á is a non-ASCII (2 byte) character zzz
    int* test = najdi(text, "ma", occurences);

    printf("Number of occurences: %i\n", *occurences);
    for (int i = 0; i < *occurences; i += 1) {
        printf(" @ index %i\n", test[i]);
    }
}