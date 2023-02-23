#ifndef BITS_H
#define BITS_H

/* Decodes the binary representation of a number from bits and returns it */
int bits2int(char* bits);

/* Encodes 'num' into its binary representation and stores it in 'bits' */
void int2bits(char* bits, int num);

void short2bits(char* bits, short num);

#endif
