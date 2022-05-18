#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define SIZE 7
#define DELETED_CHAR 127
#define DELETED_P_OAT(entry) (entry[0] == DELETED_CHAR)

/* ~~~ Structs and methods ~~~ */

typedef struct node {
    char* data;
    struct node* next;
    struct node* prev;
} node;

/* Allocates a node containing data, returns it's pointer */
node* create_node(char* data) {
    /* Allocate node */
    node* new_node = (node*) malloc(sizeof(node));

    /* Check malloc success */
    if (new_node == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate node");
        return NULL;
    }

    /* Copy data to node data */
    new_node->data = (char*) malloc(strlen(data) * sizeof(char));

    /* Check malloc success */
    if (new_node->data == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate data");
        free(new_node);
        return NULL;
    }

    /* Copy data */
    strcpy(new_node->data, data);

    /* Initialize node pointers */
    new_node->next = NULL;
    new_node->prev = NULL;

    return new_node;
}

void free_node(node* node) {
    free(node->data);
    free(node);
}

typedef struct {
    node* start;
} list;

void free_list(list* list) {
    node* current = list->start;
    node* previous;

    while (current != NULL) {
        previous = current;
        current = current->next;
        free_node(previous);
    }

    free(list);
}

typedef struct {
    list** data_lists;
    int size;
    int (*hash)(char*);
} chaining_table;

/* Allocates a chaining_table with data_lists of given size and hash function, returns it's pointer */
chaining_table* create_chaining_table(int size, int (*hash)(char*)) {
    /* Allocate table */
    chaining_table* table = (chaining_table*) malloc(sizeof(chaining_table));

    /* Check malloc success */
    if (table == NULL) {
        fprintf(stderr, "[ERROR in create_chaining_table()]: Failed to allocate table");
        return NULL;
    }

    /* Assign hash & size */
    table->hash = hash;
    table->size = size;

    /* Allocate array of data lists */
    table->data_lists = (list**) malloc(size * sizeof(list*));

    /* Check malloc success */
    if (table->data_lists == NULL) {
        fprintf(stderr, "[ERROR in create_chaining_table()]: Failed to allocate data_lists");
        free(table);
        return NULL;
    }

    /* Initialize data_lists */
    for (int i = 0; i < size; i += 1) {
        /* Allocate list */
        table->data_lists[i] = (list*) malloc(sizeof(list));

        /* Check malloc success */
        if (table->data_lists[i] == NULL) {
            fprintf(stderr, "[ERROR in create_chaining_table()]: Failed to allocate list");
            free(table->data_lists);
            free(table);
            return NULL;
        }

        /* Initialize start pointer */
        table->data_lists[i]->start = NULL;
    }

    return table;
}

/* Deallocates a chaining table */
void free_chaining_table(chaining_table* table) {
    for (int i = 0; i < table->size; i += 1) {
        free_list(table->data_lists[i]);
    }

    free(table->data_lists);
    free(table);
}

typedef struct {
    char** data;
    int data_size;
    int (*hash)(char*);
    int (*probe)(int, int);
} oa_table;

/* Allocates an open-addressing table and returns it's pointer */
oa_table* create_oa_table(int size, int (*hash)(char*), int (*probe)(int, int)) {
    /* Allocate table */
    oa_table* table = (oa_table*) malloc(sizeof(oa_table));

    /* Check malloc success */
    if (table == NULL) {
        fprintf(stderr, "[ERROR in create_oa_table()]: Failed to allocate table");
        return NULL;
    }

    /* Allocate data array */
    table->data = (char**) malloc(size * sizeof(char*));

    /* Check malloc success */
    if (table->data == NULL) {
        fprintf(stderr, "[ERROR in create_oa_table()]: Failed to allocate data");
        free(table);
        return NULL;
    }

    /* Initialize data array */
    for (int i = 0; i < size; i += 1) {
        table->data[i] = NULL;
    }

    /* Assign remaining "fields" */
    table->data_size = size;
    table->hash = hash;
    table->probe = probe;

    return table;
}

/* Deallocates an open-addressing table */
void free_oa_table(oa_table* table) {
    /* Free all allocated strings */
    for (int i = 0; i < table->data_size; i += 1) {
        free(table->data[i]);
    }

    free(table);
}

/* ~~~ Functions ~~~ */

/* Inserts a new node containing data to data_lists[index] (returns 1 if successful, 0 if not) */
int insert_node(char* data, list* data_list) {
    /* Create a new data node */
    node* new_node = create_node(data);
    if (new_node == NULL) return 0;  // In case malloc fails

    /* Insert new node to the start of data_lists[index] */
    node* old_start = data_list->start;
    new_node->next = old_start;
    if (old_start != NULL) old_start->prev = new_node;
    data_list->start = new_node;

    return 1;
}

/* Returns a pointer to the first node containing data in data_lists[index] or NULL if not found */
node* find_node(char* data, list* data_list) {
    node* current_node = data_list->start;

    while (current_node != NULL) {
        /* Return current_node if it's data is equal to data */
        if (strcmp(current_node->data, data) == 0) return current_node;

        /* Go to next node */
        current_node = current_node->next;
    }

    /* Return NULL if not found */
    return NULL;
}

/* Removes and deallocates removed node from data_list */
void remove_node(node* removed, list* data_list) {
    node* previous = removed->prev;
    node* next = removed->next;

    if (previous == NULL && next == NULL) {
        data_list->start = NULL;
    }
    else if (previous == NULL) {
        data_list->start = next;
        next->prev = NULL;
    }
    else if (next == NULL) {
        previous->next = NULL;
    }
    else {
        previous->next = next;
        next->prev = previous;
    }

    free_node(removed);
}

void print_list(list* list) {
    node* current = list->start;
    char first = 1;

    while (current != NULL) {
        if (!first) printf(", ");
        printf("%s", current->data);

        first = 0;
        current = current->next;
    }

    printf("\n");
}

/* Adds data to chaining table (returns 1 if successful, 0 if not) */
int add_ct(char* data, chaining_table* table) {
    int index = table->hash(data);
    return insert_node(data, table->data_lists[index]);  // 0 or 1
}

/* Removes (first occurence of) data from chaining table (returns 1 if successful, 0 if not) */
int remove_ct(char* data, chaining_table* table) {
    int index = table->hash(data);
    node* removed = find_node(data, table->data_lists[index]);

    /* Return 0 if to-be-removed node not found */
    if (removed == NULL) return 0;

    /* Remove node */
    remove_node(removed, table->data_lists[index]);

    return 1;
}

/* Returns 1 if chaining table contains data, 0 if not */
int contains_ct(char* data, chaining_table* table) {
    int index = table->hash(data);
    node* found = find_node(data, table->data_lists[index]);

    return (found != NULL) ? 1 : 0;
}

void print_ct(chaining_table* table) {
    printf("Chaining table:\n");
    for (int i = 0; i < table->size; i += 1) {
        printf(" - ");
        print_list(table->data_lists[i]);
    }
}

/* Adds data to oa_table (returns 1 if successful, 0 if not) */
int add_oat(char* data, oa_table* table) {
    int data_hash = table->hash(data);
    char* cur_data;
    int index;

    for (int i = 0; i < table->data_size; i += 1) {
        index = table->probe(data_hash, i);
        cur_data = table->data[index];

        if (cur_data == NULL || DELETED_P_OAT(cur_data)) {
            free(cur_data);  // free(NULL) is defined
            table->data[index] = (char*) malloc(strlen(data) + 1 * sizeof(char));
            strcpy(table->data[index], data);
            return 1;
        }
    }

    return 0;
}

/* Removes the first occurence of data in oa_table (returns 1 if successful, 0 if not) */
int remove_oat(char* data, oa_table* table) {
    int data_hash = table->hash(data);
    char* cur_data;
    int index;

    for (int i = 0; i < table->data_size; i += 1) {
        index = table->probe(data_hash, i);
        cur_data = table->data[index];

        if (cur_data != NULL && strcmp(cur_data, data) == 0) {
            cur_data[0] = DELETED_CHAR;  // Sets first char to unprintable(?) DEL
            return 1;
        }
    }

    return 0;
}

/* Returns 1 if oa_table contains data, 0 if not */
int contains_oat(char* data, oa_table* table) {
    int data_hash = table->hash(data);
    char* cur_data;
    int index;

    for (int i = 0; i < table->data_size; i += 1) {
        index = table->probe(data_hash, i);
        cur_data = table->data[index];

        if (cur_data == NULL) return 0;  // Stop on first empty field(!!!)
        if (strcmp(cur_data, data) == 0) return 1;
    }

    return 0;
}

void print_oat(oa_table* table) {
    char first = 1;
    char* cur_data;

    printf("Open-addressing table:\n - ");
    for (int i = 0; i < table->data_size; i += 1) {
        cur_data = table->data[i];

        if (!first) printf(", ");
        (cur_data == NULL || DELETED_P_OAT(cur_data)) ? printf("%s", "___") : printf("%s", cur_data);
        first = 0;
    }
    printf("\n");
}

int ascii_hash(char* text) {
    unsigned long long hash = 0;
    int text_len = strlen(text);
    int mod = SIZE;

    for (int i = 0; i < text_len; i += 1) {
        hash += text[i] * (int) pow(128, (text_len - (i + 1)));
    }

    hash = hash % mod;

    return hash;
}

int linear_probe(int hashed_data, int offset) {
    return (hashed_data + offset) % SIZE;
}

int quadratic_probe(int hashed_data, int offset) {
    return (hashed_data + offset + (offset * offset)) % SIZE;
}

void test_oat(oa_table* table) {
    add_oat("car", table);
    add_oat("aba", table);
    print_oat(table);
    add_oat("abc", table);  // "abc" collides with "car" (using ascii_hash())
    add_oat("car", table);
    print_oat(table);
    remove_oat("car", table);
    print_oat(table);
    (table->data[4] != NULL) ? printf("Data isn't NULL after deletion!\n") : printf("Something failed.\n");
    add_oat("car", table);
    print_oat(table);
    (contains_oat("abc", table)) ? printf("OAT contains abc!\n") : printf("OAT doesn't contain abc!\n");
    (contains_oat("cba", table)) ? printf("OAT contains cba!\n") : printf("OAT doesn't contain cba!\n");
}

/* ~~~ MAIN ~~~ */

int main(void) {
    // chaining_table* CT = create_chaining_table(SIZE, ascii_hash);
    // oa_table* linear_OA = create_oa_table(SIZE, ascii_hash, linear_probe);
    // oa_table* quadratic_OA = create_oa_table(SIZE, ascii_hash, quadratic_probe);

    // add_ct("car", CT);
    // add_ct("aba", CT);
    // print_ct(CT);
    // add_ct("abc", CT);  // "abc" collides with "car" (using ascii_hash())
    // print_ct(CT);
    // remove_ct("car", CT);
    // print_ct(CT);
    // (contains_ct("abc", CT)) ? printf("CT contains abc!\n") : printf("CT doesn't contain abc!\n");
    // (contains_ct("cba", CT)) ? printf("CT contains cba!\n") : printf("CT doesn't contain cba!\n");

    // printf("\n");

    // test_oat(linear_OA);

    // printf("\n");

    // test_oat(quadratic_OA);

    // free_chaining_table(CT);
    // free_oa_table(linear_OA);
    // free_oa_table(quadratic_OA);

    return 0;
}
