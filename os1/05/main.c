#include <stdio.h>
#include <stdlib.h>

// funkce z assembleru
// @see https://phoenix.inf.upol.cz/~krajcap/courses/2023LS/OS1/os1-05.htm
void swap(int *a, int *b);
void division(unsigned int x, unsigned int y, unsigned int *result, unsigned int *remainder);
void countdown(int *values);
void nasobky(short *multiples, short n);
int minimum(int count, int *values);
unsigned int my_strlen(char *s);
void my_strcat(char *dest, char *src);

int main() {
    // void swap(int*, int*)
    int x = 4, y = 2;
    int *a = &x, *b = &y;
    printf("[swap()]: x: %i, y: %i -> ", *a, *b);
    swap(a, b);
    printf("x: %i, y: %i\n\n", *a, *b);

    // void division(ui, ui, ui*, ui*)
    unsigned int dx = 1242, dy = 7, res, rem;
    division(dx, dy, &res, &rem);
    printf("[division()]: %u / %u = %u (remainder %u)\n\n", dx, dy, res, rem);

    // void countdown(int*)
    int array[10];
    countdown(array);
    printf("[countdown()]: ");
    for (int i = 0; i < 10; i += 1) {
        printf("%i ", array[i]);
    }
    printf("\n\n");

    // void nasobky(short*, short);
    short short_array[10];
    nasobky(short_array, 3);
    printf("[nasobky()]: ");
    for (int i = 0; i < 10; i += 1) {
        printf("%hi ", short_array[i]);
    }
    printf("\n\n");

    // int minimum(int, int*)
    int arr[9] = {4, 5, -99, 2039, 238, -99, 1, 1239, 1};
    int min = minimum(9, arr);
    printf("[minimum()]: %i\n\n", min);

    // unsigned int my_strlen(char*)
    char* string = "Hello, world!";
    printf("[my_strlen()]: Length of '%s' is %u\n\n", string, my_strlen(string));

    // void my_strcat(char*, char*)
    char* also_string = " And hello, everyone living in it!";
    char* dest_string = (char*) malloc(99);
    my_strcat(dest_string, string);
    my_strcat(dest_string, also_string);
    printf("[my_strcat()]: %s\n\n", dest_string);

    return 0;
}