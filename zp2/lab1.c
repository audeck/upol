/*
    Guaranteed to work if input numbers are less than 65 533 digits long.
    If longer numbers are to be input, multiple int type variables should
    be "elongated" first, guaranteeing a range of up to 18 446 744 073 709 551 613
    digits. Depends on the input data's nature though, so I'll remain in
    2-byte integer land.
*/

#include <stdio.h>
#include <stdlib.h>

#define abs(a) ((a > 0) ? a : -a)
#define sign(a) ((a < 0) ? -1 : 1)
#define max(a, b) ((a > b) ? a : b)
#define is_number(a) ((a >= '0') && (a <= '9'))  // i.e. ASCII coded number

/* Returns the length of given string */
int string_length(char* string) {
    int i;
    for (i = 0; string[i]; i += 1);
    return i;
}

/* Compares two string */
int string_compare(char* str1, char* str2) {
    for (int i = 0; str1[i] || str2[i]; i += 1) {
        if (str1[i] != str2[i]) return 0;
    }
    return 1;
}

/* Adds up two decimal(!) numbers represented as strings (char arrays) and returns the result char array's pointer */
char* secti_cisla(char* num1, char* num2) {
    int  num1_cur    = string_length(num1);
    int  num2_cur    = string_length(num2);
    char num1_sign   = (num1[0] == '-') ? -1 : 1;
    char num2_sign   = (num2[0] == '-') ? -1 : 1;
    char num1_start  = (num1[0] == '-' || num1[0] == '+') ? 1 : 0;  // 1 if the number has a sign
    char num2_start  = (num2[0] == '-' || num2[0] == '+') ? 1 : 0;
    int  output_len  = max(num1_cur, num2_cur) + 2;  // Maximum possible output length
    int  output_cur  = output_len;
    char output_sign = 1;

    /* Allocate output char array */
    char* output = (char*) malloc(sizeof(char) * output_len);

    if (output == NULL) {
        fprintf(stderr, "[ERROR in secti_cisla()]: Failed to allocate output char array");
        return NULL;
    }

    /* Initialize output to all zeros */
    for (int i = 0; i < output_len; i += 1) {
        output[i] = 0;
    }

    /* Decrement current "pointers" */
    num1_cur -= 1;
    num2_cur -= 1;
    output_cur -= 1;

    /*  
        NOTE: The output array gets first populated with coefficients of range [-19, 19] (num1 + num2),
        which then get "collapsed" according to output's sign (= highest non-zero coefficient's sign)
        to get the final result.
    */

    /* Compute output coefficients */
    while (num1_cur >= num1_start && num2_cur >= num2_start) {
        /* Check if current chars are valid numbers */
        if (is_number(num1[num1_cur]) && is_number(num2[num2_cur])) {
            /* Add up num1 and num2 coefficients */
            output[output_cur] = (num1[num1_cur] - '0') * num1_sign
                               + (num2[num2_cur] - '0') * num2_sign;

            /* Update sign if non-zero output coefficient */
            if (output[output_cur] != 0) {
                output_sign = sign(output[output_cur]);
            }

            /* Decrement index "pointers" */
            num1_cur -= 1;
            num2_cur -= 1;
            output_cur -= 1;
        }
        else {
            fprintf(stderr, "[ERROR in secti_cisla()]: Input numbers contain non-number characters");
            free(output);
            return NULL;
        }
    }

    /* Insert rest of num1 into output */
    while (num1_cur >= num1_start) {
        /* Check if current num1 char is a valid number */
        if (is_number(num1[num1_cur])) {
            /* Insert num1 coefficient */
            output[output_cur] = (num1[num1_cur] - '0') * num1_sign;

            /* Update sign if non-zero output coefficient */
            if (output[output_cur] != 0) {
                output_sign = sign(output[output_cur]);
            }

            /* Decrement index "pointers" */
            num1_cur -= 1;
            output_cur -= 1;
        }
        else {
            fprintf(stderr, "[ERROR in secti_cisla()]: Input numbers contain non-number characters");
            free(output);
            return NULL;
        }
    }

    /* Insert rest of num2 into output */
    while (num2_cur >= num2_start) {
        /* Check if current char is a valid number */
        if (is_number(num2[num2_cur])) {
            /* Insert num2 coefficient */
            output[output_cur] = (num2[num2_cur] - '0') * num2_sign;

            /* Update sign if non-zero output coefficient */
            if (output[output_cur] != 0) {
                output_sign = sign(output[output_cur]);
            }

            /* Decrement index "pointers" */
            num2_cur -= 1;
            output_cur -= 1;
        }
        else {
            fprintf(stderr, "[ERROR in secti_cisla()]: Input numbers contain non-number characters");
            free(output);
            return NULL;
        }
    }

    int remainder = 0;

    /* "Collapse" coefficients depending on output sign (which side of the number line output is) */
    if (output_sign == 1) {
        for (int i = output_len - 1; i >= 0; i -= 1) {
            output[i] += remainder;

            if (output[i] > 9) {
                remainder = 1;
                output[i] = '0' + abs(output[i]) - 10;
            }
            else if (output[i] < 0) {
                remainder = -1;
                output[i] = '0' + 10 - abs(output[i]);
            }
            else {
                remainder = 0;
                output[i] = '0' + abs(output[i]);
            }
        }
    }
    else {
        for (int i = output_len - 1; i >= 0; i -= 1) {
            output[i] += remainder;

            if (output[i] < -9) {
                remainder = -1;
                output[i] = '0' + abs(output[i]) - 10;
            }
            else if (output[i] > 0) {
                remainder = 1;
                output[i] = '0' + 10 - abs(output[i]);
            }
            else {
                remainder = 0;
                output[i] = '0' + abs(output[i]);
            }
        }
    }

    /* Insert + or - sign */
    output[0] = ((output_sign == 1) ? '+' : '-');

    /* Initialize output_shift to 0 or 1 (in order to omit + sign) */
    int output_shift = (output_sign == 1); 

    /* Increment output_shift to omit leading zeros */
    for (int i = 1; i < output_len - 1; i += 1) {
        if (output[i] == '0') {
            output_shift += 1;
        }
        else {
            break;
        }
    }

    /* Shift output chars by output_shift (including terminating 0) */
    if (output_shift > 0) {
        for (int i = output_shift + (output_sign == -1); i <= output_len; i += 1) {
            output[i - output_shift] = output[i]; 
        }
    }

    /*  
        NOTE: Could also realloc to free the non-used chars, but reallocating
        to a smaller size is finicky, so I just "move" the terminating 0.
    */

    return output;
}

int main(void) {
    // char test1_1[] = "+10000";
    // char test1_2[] = "-1";
    // char* test1 = secti_cisla(test1_1, test1_2);  // 9999
    // if (string_compare(test1, "9999")) {
    //     printf("[✓] Test 1 passed: %s + %s = %s\n", test1_1, test1_2, test1);
    // }
    // else {
    //     printf("[X] Test 1 failed: %s + %s ≠ %s (should be 9999)\n", test1_1, test1_2, test1);
    // }

    // char test2_1[] = "-10000";
    // char test2_2[] = "1";
    // char* test2 = secti_cisla(test2_1, test2_2);  // -9999
    // if (string_compare(test2, "-9999")) {
    //     printf("[✓] Test 2 passed: %s + %s = %s\n", test2_1, test2_2, test2);
    // }
    // else {
    //     printf("[X] Test 2 failed: %s + %s ≠ %s (should be -9999)\n", test2_1, test2_2, test2);
    // }

    // char test3_1[] = "9999";
    // char test3_2[] = "1";
    // char* test3 = secti_cisla(test3_1, test3_2);  // 10000
    // if (string_compare(test3, "10000")) {
    //     printf("[✓] Test 3 passed: %s + %s = %s\n", test3_1, test3_2, test3);
    // }
    // else {
    //     printf("[X] Test 3 failed: %s + %s ≠ %s (should be 10000)\n", test3_1, test3_2, test3);
    // }

    // char test4_1[] = "-9999";
    // char test4_2[] = "-1";
    // char* test4 = secti_cisla(test4_1, test4_2);  // -10000
    // if (string_compare(test4, "-10000")) {
    //     printf("[✓] Test 4 passed: %s + %s = %s\n", test4_1, test4_2, test4);
    // }
    // else {
    //     printf("[X] Test 4 failed: %s + %s ≠ %s (should be -10000)\n", test4_1, test4_2, test4);
    // }

    // char test5_1[] = "+1";
    // char test5_2[] = "-1";
    // char* test5 = secti_cisla(test5_1, test5_2);  // 0
    // if (string_compare(test5, "0")) {
    //     printf("[✓] Test 5 passed: %s + %s = %s\n", test5_1, test5_2, test5);
    // }
    // else {
    //     printf("[X] Test 5 failed: %s + %s ≠ %s (should be 0)\n", test5_1, test5_2, test5);
    // }

    // free(test1);
    // free(test2);
    // free(test3);
    // free(test4);
    // free(test5);

    return 0;
}
