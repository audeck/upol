#ifndef DATES_H
#define DATES_H

short encode_date(char day, char month, short year);

void decode_date(short date, int *day, int *month, int *year);

#endif
