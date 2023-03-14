#include <stdio.h>
#include <stdlib.h>



char* char2flags(char num) {
    // Allocate memory for the binary string (including the null terminator)
    char* bin = malloc(sizeof(char) * 5);
    if (bin == NULL) {
        fprintf(stderr, "Error: failed to allocate memory for binary string\n");
        exit(1);
    }
    
    // Convert the number to binary and store it in the string
    int i;

    for (i = 3; i >= 0; i -= 1) {
        bin[3-i] = (num & (1 << i)) ? '1' : '0';
    }

    // Add a null terminator to the end of the string
    bin[4] = '\0';
    
    return bin;
}



/* prototypy funkci z assembly */
int sgn(int i);
char max2c(char a, char b);
unsigned short min3us(unsigned short a, unsigned short b, unsigned short c);
int kladne(int a, int b, int c);
int mocnina(int n, unsigned int m);
char flag_test(char a, char b);
char oflag_test();



int main() {
    // sgn()
    int i = -123;
    printf("Sign of %i: %i\n", i, sgn(i));
    i = 0;
    printf("Sign of %i: %i\n", i, sgn(i));
    i = 111;
    printf("Sign of %i: %i\n\n", i, sgn(i));

    // max2c()
    char a = 122, b = 123;
    printf("Max of %i, %i: %i\n", a, b, max2c(a, b));
    b = 78;
    printf("Max of %i, %i: %i\n\n", a, b, max2c(a, b));

    // min3us()
    unsigned short c = 33333, d = 34567, e = 42424;
    printf("Minimum of %u, %u, %u: %u\n", c, d, e, min3us(c, d, e));
    c = e, e = 33333;
    printf("Minimum of %u, %u, %u: %u\n\n", c, d, e, min3us(c, d, e));

    // kladne()
    int f = 12, g = 0, h = -2;
    printf("Kladne(%i, %i, %i): %i\n", f, g, h, kladne(f, g, h));
    h = 2;
    printf("Kladne(%i, %i, %i): %i\n", f, g, h, kladne(f, g, h));
    g = 1;
    printf("Kladne(%i, %i, %i): %i\n\n", f, g, h, kladne(f, g, h));

    // mocnina()
    int k = 2, l = 4;
    printf("%i ^ %u = %i\n", k, l, mocnina(k, l));
    l = 0;
    printf("%i ^ %u = %i\n\n", k, l, mocnina(k, l));

    // flag_test()
    char x = 14, y = 13;
    char* res = char2flags(flag_test(x, y));
    printf("Flags set by %i + %i are: %s [OF:CF:SF:ZF]\n", x, y, res);
    y = -14, res = char2flags(flag_test(x, y));
    printf("Flags set by %i + %i are: %s [OF:CF:SF:ZF]\n", x, y, res);
    y = -19, res = char2flags(flag_test(x, y));
    printf("Flags set by %i + %i are: %s [OF:CF:SF:ZF]\n", x, y, res);
    x = -128, y = -127, res = char2flags(flag_test(x, y));
    printf("Flags set by %i + %i are: %s [OF:CF:SF:ZF]\n", x, y, res);
    res = char2flags(oflag_test());
    printf("Flags set by 127 + 1 are: %s [OF:CF:SF:ZF]\n", res);  // ?
    free(res);

    return 0;
}