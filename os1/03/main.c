#include <stdio.h>

/* prototypy funkci z assembly */
int obsah_obdelnika(int a, int b);
int obvod_ctverce(int a);
int obsah_ctverce(int a);
int obvod_trojuhelnika(int a, int b, int c);
int obvod_trojuhelnika2(int a);
int obsah_trojuhelnika2(int a, int b);
int obsah_trojuhelnika3(int a, int va);
int objem_krychle(int a);
unsigned int avg(unsigned int a, unsigned int b, unsigned int c);

int main() {
    printf("Obsah obdelnika 3x4:      %i (should be 12)\n", obsah_obdelnika(3, 4));
    printf("Obvod ctverce 6x6:        %i (should be 24)\n", obvod_ctverce(6));
    printf("Obsah ctverce 6x6:        %i (should be 36)\n", obsah_ctverce(6));
    printf("Obvod trojuhelnika 2x3x4:  %i (should be 9)\n", obvod_trojuhelnika(2, 3, 4));
    printf("Obvod trojuhelnika 3x3x3:  %i (should be 9)\n", obvod_trojuhelnika2(3));
    printf("Obsah trojuhelnika 3x4x5:  %i (should be 6)\n", obsah_trojuhelnika2(3, 4));
    printf("Obsah trojuhelnika b4h3:   %i (should be 6)\n", obsah_trojuhelnika3(4, 3));
    printf("Objem krychle 4x4x4:      %i (should be 64)\n", objem_krychle(4));
    printf("Prumer cisel 6, 18, 9:    %u (should be 11)\n", avg(6, 18, 9));

    return 0;
}