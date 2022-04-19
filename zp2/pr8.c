#include <stdio.h>
#include <stdlib.h>

double na0(double x) {
    return 1;
}

double na1(double x) {
    return x;
}

double na2(double x) {
    return x*x;
}

double na3(double x) {
    return x*x*x;
}

double na4(double x) {
    return x*x*x*x;
}

double *map(double (*fce)(double), double *vstup, int pocet){
    int i;
    double *vystup = malloc(pocet*sizeof(double));
    for(i=0; i<pocet; i++)
    {
        vystup[i]=fce(vstup[i]);
    }
    return vystup;
}

void print_array(double* array, int pocet) {
    for (int i = 0; i < pocet; i += 1) {
        printf("%.0lf", array[i]);
        if (i < pocet - 1) printf(", ");
    }
    printf("\n");
}

double soucet(double x, double y) {
    return x + y;
}

double soucin(double x, double y) {
    return x * y;
}

double akumulator(double (*fce)(double, double), double cisla[], int pocet) {
    if (pocet == 0) return 0;

    double result = cisla[0];

    for (int i = 1; i < pocet; i += 1) {
        result = fce(result, cisla[i]);
    }

    return result;
}



int main(void) {
    double (*pole_funkci[5])(double) = {na0, na1, na2, na3, na4};

    printf("1. příklad:\n");

    printf(" - (-1)^1 = %.0lf\n", pole_funkci[1](-1));
    printf(" - (-1)^2 = %.0lf\n", pole_funkci[2](-1));
    printf(" - 2^3 = %.0lf\n", pole_funkci[3](2));

    printf("2. příklad:\n");

    double cisla[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    double *mocniny[5];

    for (int i = 0; i < 5; i += 1) {
        mocniny[i] = map(pole_funkci[i], cisla, 10);
        printf(" - mocniny %i.: ", i);
        print_array(mocniny[i], 10);
    }

    printf("5. příklad:\n");

    printf(" - suma je: %g\n", akumulator(soucet, cisla, 10));
    printf(" - produkt je: %g\n", akumulator(soucin, cisla, 10));

    return 0;
}