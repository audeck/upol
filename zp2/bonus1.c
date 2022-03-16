/*
    I left the assignment "test" uncommented by accident :^).
    Either way; I don't think there was anything code-crashing
    wrong with 'occurences'. At first I thought so and went
    on to create a Python script to check if the code crashes
    (which I'm including as well for... you know, style points),
    as I didn't experience a single crash while working on this.
    Then, after learning absolutely nothing, I thought about it 
    and came to the conclusion that I'm missing a return statement,
    the value of which automated tests usually check (which is only
    a problem with pre-C99 standard compilers to be fair). I also
    changed the 'occurences' declaration to explicitly allocate
    memory (in case declarations don't do that in older standards
    either) and also added memory releasing to clean up.
    
    Also also - the test is now commented (as it should be) :^))).

    On the off chance that whatever problem you ran into still
    persists, it's safe to say that I'm stumped.
*/

#include <stdio.h>
#include <stdlib.h>

/* 
    Assigns the amount of 'podretezec' occurences in 'text' to memory at 
    'pocet_vyskytu' and returns a pointer to an array of their indices. 
    (!) Case insensitivity (which, while not explicitly required by the 
    assignment, makes sense) is guaranteed only for 1-byte ASCII characters.
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
                comparison, but I am not straying away from 1-byte characters >:).
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
    // int* occurences = malloc(sizeof(int));
    // char* text = "Máma mele maso~";  // á is a non-ASCII (2 byte) character zzz
    // int* test = najdi(text, "ma", occurences);

    // printf("Number of occurences: %i\n", *occurences);
    // for (int i = 0; i < *occurences; i += 1) {
    //     printf(" @ index %i\n", test[i]);
    // }

    // free(occurences);
    // free(test);

    return 0;
}