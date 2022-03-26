#include <stdio.h>
#include <stdlib.h>

/* I store an array's size in it's "-1st element" (array of n elements is therefore size n+1) */

int sequence_search(int array[], int element) {
    /* Array size */
    array--; int len = *array; array++;

    for (int i = 0; i < len; i++) {
        if (array[i] == element) {
            return i;
        }
    }

    return -1;
}

/* 'array' has to be sorted (!) */
int binary_search(int array[], int element) {
    /* Array size */
    array--; int len = *array; array++;

    int l = 0;
    int r = len - 1;
    int s;

    while (l <= r) {
        s = (l + r) / 2;
        printf("L: %i, R: %i, S: %i\n", l, r, s);
        if (array[s] == element) return s;
        if (array[s] > element) r = s - 1;
        if (array[s] < element) l = s + 1;
    }

    return -1;
}

/* 'array' has to be sorted (!) */
int interpolation_search(int array[], int element) {
    /* Array size */
    array--; int len = *array; array++;

    int l = 0;
    int r = len - 1;
    int s;

    while (l <= r) {
        s = (element - array[l]) / (array[r] - array[l]) * (r - l) + l;
        if (array[s] == element) return s;
        if (array[s] > element) r = s - 1;
        if (array[s] < element) l = s + 1;
    }

    return -1;
}



int main(void) {
    // int arr1[5] = {1, 2, 4, 7, 9};
    // int *array1 = malloc(6 * sizeof(int));
    // array1[0] = 5;  // Size of array1
    // array1++;  // Increment pointer to point to the first real element
    // for (int i = 0; i < 5; i++) {
    //     array1[i] = arr1[i];
    // }

    // int res = sequence_search(array1, 2);
    // int resB = binary_search(array1, 9);
    // int resI = interpolation_search(array1, 3);
    // printf("%i ", res);
    // printf("%i ", resB);
    // printf("%i\n", resI);
    
    // array1--;
    // free(array1);
    
    return 0;
}
