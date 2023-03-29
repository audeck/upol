#include <stdio.h>

// Funkce napsane v assembly
void print_row(int n, char c);
void print_rect(int rows, int cols);
unsigned int factorial(unsigned int n);
char* my_strdup(char* s);
unsigned int fib(unsigned short n);
void print_facts(unsigned char n);

// Pomocna funkce z textu
void printi(int n) { printf("%i\n", n); }

int main() {
    // print_row
    printf("[print_row]:\n");
    print_row(5, '+');
    printf("\n");

    // print_rect
    printf("[print_rect]:\n");
    print_rect(5, 5);
    printf("\n");

    // factorial
    printf("[factorial]:\n");
    unsigned int a = 6;
    printf("Factorial of %u is %u\n", a, factorial(a));
    printf("\n");

    // my_strdup
    printf("[my_strdup]:\n");
    char* my_string = "Hello, world!";
    printf("Original: %s\n", my_string);
    printf("Copy: %s\n", my_strdup(my_string));
    printf("\n");

    // fib
    printf("[fib]:\n");
    unsigned short f = 19;
    printf("Fibonnaci number #%hu is %u\n", f, fib(f));
    printf("\n");

    // print_facts
    printf("[printf_facts]:\n");
    print_facts(6);

    return 0;
}