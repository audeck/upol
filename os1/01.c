#include <stdio.h>
#include <stdlib.h>
#include <string.h>



/****************************** UKOL #1 ******************************/

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



/****************************** UKOL #2 ******************************/

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



/****************************** UKOL #3 ******************************/

#define MAX_DAY 31
#define MAX_MONTH 12
#define YEAR_OFFSET 1900
#define MAX_YEAR (YEAR_OFFSET + 127)  // 1900 + 2^7 - 1

#define DAY_BITS 5
#define MONTH_BITS 4
#define YEAR_BITS 7

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



/****************************** UKOL #4 ******************************/

#define DAY_MASK 31  // 0b11111
#define MONTH_MASK 480  // 0b111100000

/* Stores the date stored in 'date' (bit format YYYY-YYYM-MMMD-DDDD) in 'day', 'month' and 'year'. */
void decode_date(short date, int* day, int* month, int* year) {
    unsigned short date_code = date;
    *year = (date_code >> (MONTH_BITS + DAY_BITS)) + YEAR_OFFSET;
    *month = (date_code & MONTH_MASK) >> DAY_BITS;
    *day = date_code & DAY_MASK;
}



/****************************** UKOL #5 ******************************/

/* 
 * Copies 'size' amount of bytes at 'src' over to 'dest'. Behavior if memory blocks
 * overlap is undefined.
 */
void my_memcpy(void *dest, void *src, size_t size) {
    // Get casted pointers that span 1 byte
    char* byte_dest = (char*) dest;
    char* byte_src = (char*) src;

    // Copy data over using pointer arithmetic
    for (int i = 0; i < size; i += 1) {
        *(byte_dest + i) = *(byte_src + i);
    }
}



/****************************** UKOL #6 ******************************/

typedef struct ll_node {
    char* name;
    int age;
    struct ll_node* next;
} ll_node;

/* Allocates a linked list node containing 'name' and 'age' and returns it's pointer. */
ll_node* ll_create_node(char* name, int age) {
    ll_node* node = (ll_node*) malloc(sizeof(ll_node));
    node->name = name;
    node->age = age;
    node->next = NULL;
    return node;
}

typedef struct {
    ll_node* head;
} linked_list;

/* Allocates a linked list and returns it's pointer. */
linked_list* ll_create_list() {
    return (linked_list*) malloc(sizeof(linked_list));
}

/* Inserts a node at the end of a given linked list. */
void ll_insert(linked_list* list, ll_node* node) {
    // Empty list
    if (list->head == NULL) {
        list->head = node;
        return;
    }

    // Find last node
    ll_node* current = list->head;
    while (current->next != NULL) {
        current = current->next;
    }

    // Append node
    current->next = node;
}

/* Prints out the contents of a linked list to stdout. */
void ll_print(linked_list* list) {
    // Empty list
    if (list->head == NULL) {
        return;
    }

    printf("Linked list: {\n");

    ll_node* current = list->head;
    while (current != NULL) {
        printf("  Name: %s; Age: %i\n", current->name, current->age);
        current = current->next;
    }

    printf("}\n");
}

/* Frees a given linked list on the heap, along with all of it's nodes. */
void ll_free(linked_list* list) {
    if (list->head == NULL) {
        free(list);
        return;
    }

    ll_node* current = list->head;
    ll_node* next = current->next;

    while (next != NULL) {
        free(current);
        current = next;
        next = current->next;
    }

    free(next);
    free(list);
}



/****************************** UKOL #7 ******************************/

int sum(int a, int b, int c, int d) {
    return a + b + c + d;
}

int f1() {
    printf(" - argument 1 evaluated!\n");
    return 1;
}

int f2() {
    printf(" - argument 2 evaluated!\n");
    return 2;
}

int f3() {
    printf(" - argument 3 evaluated!\n");
    return 3;
}

int f4() {
    printf(" - argument 4 evaluated!\n");
    return 4;
}

void arg_order_test() {
    for (int i = 0; i < 3; i += 1) {
        printf("Argument evaluation:\n");
        sum(f1(), f2(), f3(), f4());
    }
}



/****************************** ADDITIONALS ******************************/

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



/****************************** MAIN ******************************/

int main() {
    // Ukol 1
    int number = -1365;
    char bits[33];
    int2bits(bits, number);
    printf("The binary representation of %i is %s.\n", number, bits);

    // Ukol 2
    int dec = bits2int(bits);
    printf("Back to decimal: %i\n", dec);

    // Ukol 3
    short date = encode_date(6, 4, 2001);
    char date_bits[17];
    short2bits(date_bits, date);
    printf("Encoded date: %s\n", date_bits);

    // Ukol 4
    int day;
    int month;
    int year;
    decode_date(date, &day, &month, &year);
    printf("The original date was: %i-%i-%i\n", day, month, year);

    // Ukol 6
    linked_list* list = ll_create_list();
    ll_node* node1 = ll_create_node("Adam", 10);
    ll_node* node2 = ll_create_node("Byron", 20);
    ll_node* node3 = ll_create_node("Celia", 30);
    ll_node* node4 = ll_create_node("Diana", 40);

    ll_insert(list, node1);
    ll_insert(list, node2);
    ll_insert(list, node3);
    ll_insert(list, node4);

    ll_print(list);
    ll_free(list);

    // Ukol 7
    arg_order_test();

    return 0;
}
