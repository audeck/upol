#include <stdio.h>

#define MAX_DAY 31
#define MAX_MONTH 12
#define YEAR_OFFSET 1900
#define MAX_YEAR (YEAR_OFFSET + 127)  // 1900 + 2^7 - 1

#define DAY_BITS 5
#define MONTH_BITS 4
#define YEAR_BITS 7
#define DAY_MASK 31  // 0b11111
#define MONTH_MASK 480  // 0b111100000

/* 
 * Encodes a date into a 16-bit(!) short in the format YYYY-YYYM-MMMD-DDDD and returns it.
 * Stores dates starting from 01-01-1900, up to 31-12-2027 (DD-MM-YYYY, inclusive).
 * Returns 0 if the input date is out of range (which doesn't guarantee it's validity).
 */
short encode_date(char day, char month, short year) {
    unsigned short output = 0;

    if ((day < 1) || (day > MAX_DAY) || (month < 1) || (month > MAX_MONTH) || (year < YEAR_OFFSET) || (year > MAX_YEAR)) {
        fprintf(stderr, "[ERROR in encode_date(char, char, short)]: Invalid date.");
        return output;
    }

    output += year - YEAR_OFFSET;
    output <<= MONTH_BITS;
    output += month;
    output <<= DAY_BITS;
    output += day;

    return output;
}



/* Stores the date stored in 'date' (bit format YYYY-YYYM-MMMD-DDDD) in 'day', 'month' and 'year'. */
void decode_date(short date, int* day, int* month, int* year) {
    unsigned short date_code = date;
    *year = (date_code >> (MONTH_BITS + DAY_BITS)) + YEAR_OFFSET;
    *month = (date_code & MONTH_MASK) >> DAY_BITS;
    *day = date_code & DAY_MASK;
}