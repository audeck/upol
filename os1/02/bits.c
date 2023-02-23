#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* 
 * Writes a null-terminated binary representation of 'num' (without it's leading 
 * zero bits) into 'bits'. Behavior if 'bits' cannot hold enough chars is undefined.
 */
void int2bits(char* bits, int num) {
    // Get amount of chars needed to hold 'num'
    size_t num_size = sizeof(num) * 8;

    // Write num's bits to 'bits'
    for (int i = 0; i < num_size; i += 1) {
        bits[i] = ((num >> (num_size - 1 - i)) & 1) + '0';
    }

    // Null-terminate 'bits'
    bits[num_size] = '\0';
}



/* 
 * Returns the integer value of a binary representation stored in 'bits'.
 * Returns 0 if 'bits' contains more chars than can be held in an integer.
 */
int bits2int(char* bits) {
    size_t bits_size = strlen(bits);
    int output = 0;
    int mask = 0x01;

    // Check if the integer type can hold 'bits'
    if (bits_size > sizeof(int) * 8) {
        fprintf(stderr, "[ERROR in bits2int(char*)]: The integer type cannot hold binary value stored in bits.\n");
        return 0;
    }

    // Flip output integer bits according to 'bits'
    for (int i = bits_size - 1; i >= 0; i -= 1) {
        // Flip bit if needed
        if (bits[i] == '1') {
            output = output | mask;
        }
        // Left shift mask over to the next bit
        mask = mask << 1;
    }

    return output;
}

/* 
 * Writes a null-terminated binary representation of 'num' (without it's leading 
 * zero bits) into 'bits'. Behavior if 'bits' cannot hold enough chars is undefined.
 */
void short2bits(char* bits, short num) {
    // Get amount of chars needed to hold 'num'
    size_t integer_size = sizeof(num) * 8;

    // Write num's bits to 'bits'
    for (int i = 0; i < integer_size; i += 1) {
        bits[i] = ((num >> (integer_size - 1 - i)) & 1) + '0';
    }

    // Null-terminate 'bits'
    bits[integer_size] = '\0';
}