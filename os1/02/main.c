#include <stdio.h>
#include <stdlib.h>
#include "bits.h"
#include "dates.h"

int rectangle_circumference();

int main() {
    // Bits.c
    int number = -1365;
    char bits[33];
    int2bits(bits, number);
    printf("The binary representation of %i is %s.\n", number, bits);

    int dec = bits2int(bits);
    printf("Back to decimal: %i\n", dec);

    // Dates.c
    short date = encode_date(6, 4, 2001);
    char date_bits[17];
    short2bits(date_bits, date);
    printf("Encoded date: %s\n", date_bits);

    int day;
    int month;
    int year;
    decode_date(date, &day, &month, &year);
    printf("The original date was: %i-%i-%i\n", day, month, year);

    // Circ.asm
    printf("The circumference of a 10x17 rectangle is: %i\n", rectangle_circumference());

    return 0;
}