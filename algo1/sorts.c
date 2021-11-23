#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

#define VEL 10  /* Zde nastavte velikost pole */
#define UPPER_BOUND 100

#define parent(i) ((i - 1) / 2)
#define left(i) ((2 * i) + 1)
#define right(i) ((2 * i) + 2)

/* Nahodne naplni pole nachystane pro "velikost" prvku hodnotami 0-99 */
void naplnPole(int pole[], int velikost) {
    time_t t;
    srand((unsigned) time(&t));
    for (int i = 0; i < velikost; i++) {
        pole[i] = rand() % UPPER_BOUND;  /* % 100 zajistuje rozsah 0-99. pro rozsah 0-1000 pouzijte % 1001 */
    }
}

/* Vypise pole obshaujici "velikost" prvku */
void vypisPole(int pole[], int velikost) {
    for (int i  =  0; i < velikost; i++) {
        printf("%d, ", pole[i]);
    }
    printf("\n");
}

/* 
    Note: in order to use multiple sorts on the original array I won't be changing
    the original array nor swapping its pointer, only printing its (local) sorted version.
 */

void insertionSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    for (int i = 1; i < velikost; i++) {
        int tmp = array[i];
        int j = i - 1;

        while (tmp < array[j] && j >= 0) {
            array[j + 1] = array[j];
            j--;
        }

        array[j + 1] = tmp;
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void selectionSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    for (int i = 0; i < velikost; i++) {
        int min_index = i;

        for (int j = i + 1; j < velikost; j++) {
            if (array[j] < array[min_index]) {
                min_index = j;
            }
        }

        int tmp = array[i];
        array[i] = array[min_index];
        array[min_index] = tmp;
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void bubbleSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    for (int i = 0; i < velikost - 1; i++) {
        for (int j = velikost - 1; j > i; j--) {
            if (array[j] < array[j - 1]) {
                int tmp = array[j];
                array[j] = array[j - 1];
                array[j - 1] = tmp;
            }
        }
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void bubbleSortImproved(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    for (int i = 0; i < velikost - 1; i++) {
        for (int j = velikost - 1; j > i; j--) {
            int last_sorted_index = j - 1;
            if (array[j] < array[j - 1]) {
                int tmp = array[j];
                array[j] = array[j - 1];
                array[j - 1] = tmp;
                last_sorted_index = j - 1;
            }
        }
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

/* Omnidirectional bubble sort */
void shakerSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    for (int i = 0; i <= velikost / 2; i++) {
        for (int j = velikost - (1 + i); j > i; j--) {
            if (array[j] < array[j - 1]) {
                int tmp = array[j];
                array[j] = array[j - 1];
                array[j - 1] = tmp;
            }
        }

        for (int j = i + 1; j < velikost - (1 + i); j++) {
            if (array[j] > array[j + 1]) {
                int tmp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = tmp;
            }
        }
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

int quickSortPartition(int array[], int start_index, int end_index) {
    int pivot = array[end_index];
    int pivot_index = start_index - 1;

    for (int i = start_index; i < end_index; i++) {
        if (array[i] <= pivot) {
            pivot_index++;
            int tmp = array[i];
            array[i] = array[pivot_index];
            array[pivot_index] = tmp;
        }
    }

    pivot_index++;
    array[end_index] = array[pivot_index];
    array[pivot_index] = pivot;

    return pivot_index;
}

void quickSortInternal(int array[], int start_index, int end_index) {
    if (start_index < end_index) {
    int pivot_index = quickSortPartition(array, start_index, end_index);
    quickSortInternal(array, start_index, pivot_index - 1);
    quickSortInternal(array, pivot_index + 1, end_index);
    }
}

void quickSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    quickSortInternal(array, 0, velikost - 1);

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void mergeSortMerge(int array[], int start_index, int half, int end_index) {
    int n1 = half - start_index + 1;
    int n2 = end_index - half;
    int *array1 = malloc(n1 + 1);
    int *array2 = malloc(n2 + 1);

    for (int i = 0; i < n1; i++) {
        array1[i] = array[start_index + i];
    }

    for (int i = 0; i < n2; i++) {
        array2[i] = array[half + 1 + i];
    }

    array1[n1] = UPPER_BOUND + 1;
    array2[n2] = UPPER_BOUND + 1;

    int i = 0;
    int j = 0;

    for (int k = start_index; k <= end_index; k++) {
        if (array1[i] < array2[j]) {
            array[k] = array1[i];
            i++;
        } else {
            array[k] = array2[j];
            j++;
        }
    }

    free(array1);
    free(array2);
}

void mergeSortInternal(int array[], int start_index, int end_index) {
    if (start_index < end_index) {
        int half = (start_index + end_index) / 2;
        mergeSortInternal(array, start_index, half);
        mergeSortInternal(array, half + 1, end_index);
        mergeSortMerge(array, start_index, half, end_index);
    }
}

void mergeSort(int pole[], int velikost) {
    /* Clone the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    mergeSortInternal(array, 0, velikost - 1);

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void maxHeapify(int *array, int size, int index) {
    int left = left(index);
    int right = right(index);
    int largest;

    if (left < size && array[left] > array[index]) {
        largest = left;
    } else {
        largest = index;
    }

    if (right < size && array[right] > array[index]) {
        largest = right;
    }

    if (largest != index) {
        int tmp = array[index];
        array[index] = array[largest];
        array[largest] = tmp;
        maxHeapify(array, size, largest);
    }
}

void buildMaxHeap(int array[], int size) {
    for (int i = (size / 2); i >= 0; i--) {
        maxHeapify(array, size, i);
    }
}

void heapSort(int pole[], int velikost) {
    /* Copy the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    buildMaxHeap(array, velikost);
    int current_size = velikost;

    for (int i = current_size - 1; i > 0; i--) {
        int tmp = array[0];
        array[0] = array[i];
        array[i] = tmp;
        current_size--;
        maxHeapify(array, current_size, 0);
    }

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

void countingSort(int array[], int digit_index) {
    int possible_digits = 10;
    int *final_array = malloc(VEL * sizeof(int));
    int *count_array = malloc(possible_digits);

    for (int i = 0; i < possible_digits; i++) {
        count_array[i] = 0;
    }

    printf("%i ", (int) pow(10, digit_index));
    printf("%i", (array[0] / (10 ^ digit_index)) % 10);

    for (int i = 0; i < VEL - 1; i++) {
        count_array[(array[i] / (10 ^ digit_index)) % 10] += 1;
    }

    for (int i = 1; i < possible_digits; i++) {
        count_array[i] += count_array[i - 1];
    }

    for (int i = VEL - 1; i >= 0; i--) {
        final_array[count_array[(array[i] / (10 ^ digit_index)) % 10] - 1] = array[i];
        count_array[(array[i] / (10 ^ digit_index)) % 10] -= 1;
    }

    int *old_pointer = array;
    array = final_array;
    free(old_pointer);
    free(count_array);
}

void radixSortInternal(int array[], int digits) {
    for (int i = 0; i < digits; i++) {
        countingSort(array, i);
    }
}

void radixSort(int pole[], int velikost) {
    /* Copy the original array */
    int *array = malloc(velikost * sizeof(int));
    memcpy(array, pole, velikost * sizeof(int));

    /* Sort */
    radixSortInternal(array, (int) log10(UPPER_BOUND));

    /* Print the (hopefully) sorted array */
    vypisPole(array, velikost);
    free(array);
}

int main(void) {
    /* Vytvorime, naplnime a vypiseme pole */
    int pole[VEL];
    naplnPole(pole, VEL);
    printf("Original:                ");
    vypisPole(pole, VEL);

    /* Sorts */
    printf("Insertion sort:           ");
    insertionSort(pole, VEL);
    printf("Selection sort:           ");
    selectionSort(pole, VEL);
    printf("Bubble sort:              ");
    bubbleSort(pole, VEL);
    printf("Improved bubble sort:     ");
    bubbleSortImproved(pole, VEL);
    printf("Shaker sort:              ");
    shakerSort(pole, VEL);
    printf("Quick sort:               ");
    quickSort(pole, VEL);
    printf("Merge sort:               ");
    mergeSort(pole, VEL);
    // printf("Heap sort:                ");
    // heapSort(pole, VEL);
    printf("Radix (w/ counting) sort: ");
    radixSort(pole, VEL);
    
    /* End of program */
    printf("EOP check:               ");
    vypisPole(pole, VEL);  /* Print original array to make sure it's unchanged */
    //getchar();  /* Pockame na "enter" a skoncime */
    return 0;
}
