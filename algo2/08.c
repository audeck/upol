#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define SIZE 127

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

typedef struct list {
    node* start;
} list;

list* create_list(node* start_node) {
    list* new_list = (list*) malloc(sizeof(list));

    if (new_list == NULL) {
        fprintf(stderr, "[ERROR in create_list()]: Failed to allocate node");
        return NULL;
    }

    new_list->start = start_node;

    return new_list;
}

void free_list(list* list) {
    node* current = list->start;
    node* next = (current == NULL) ? NULL : current->next;

    while (next != NULL) {
        free(current);
        current = next;
        next = current->next;
    }

    // In case initial next assignment is NULL (free(NULL) is defined behaviour)
    free(current);
}

typedef struct chaining_table {
    list** data_lists;
    int    size;
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

    /* Assign hash & data size */
    table->size = size;
    table->hash = hash;

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
        table->data_lists[i] = create_list(NULL);

        /* Check malloc success */
        if (table->data_lists[i] == NULL) {
            fprintf(stderr, "[ERROR in create_chaining_table()]: Failed to allocate a data_list");
            for (int j = i - 1; j >= 0; j -= 1) {
                free(table->data_lists[j]);
            }
            free(table->data_lists);
            free(table);
        } 
    }

    return table;
}

/* Deallocates a chaining table */
void free_chaining_table(chaining_table* table) {
    int size = table->size;

    for (int i = 0; i < size; i += 1) {
        free_list(table->data_lists[i]);
    }
}

typedef struct oa_table {
    char** data;
    int    size;
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
    table->size = size;
    table->hash = hash;
    table->probe = probe;

    return table;
}

/* Deallocates an open-addressing table */
void free_oa_table(oa_table* table) {
    /* Free all allocated strings */
    for (int i = 0; i < table->size; i += 1) {
        free(table->data[i]);
    }

    free(table);
}

/* Inserts a new node containing data to data_list (returns 1 if successful, 0 if not) */
int insert_node(char* data, list* data_list) {
    /* Create a new data node */
    node* new_node = create_node(data);
    if (new_node == NULL) return 0;  // In case malloc fails

    /* Insert new node at the start of data_list */
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
        free_node(removed);
    }
    else if (previous == NULL) {
        data_list->start = next;
        next->prev = NULL;
        free_node(removed);
    }
    else if (next == NULL) {
        previous->next = NULL;
        free_node(removed);
    }
    else {
        previous->next = next;
        next->prev = previous;
        free_node(removed);
    }
}

/* Adds data to chaining table (returns 1 if successful, 0 if not) */
int add_ct(char* data, chaining_table* table) {
    int index = table->hash(data);
    return insert_node(data, table->data_lists[index]);
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

int ascii_hash(char* text) {
    unsigned long long hash = 0;
    int text_len = strlen(text);
    int mod = SIZE;

    for (int i = 0; i <= text_len; i += 1) {
        hash += text[i] * (int) pow(128, (text_len - (i + 1)));
    }

    hash = hash % mod;

    return hash;
}

int add_oat(char* data, oa_table* table) {
    int init_hash = table->hash(data);

    for (int i = 0; i < table->size; i += 1) {
        int index = table->probe(init_hash, i);

        if (table->data[index] == NULL) {
            int len = strlen(data) + 1;
            table->data[index] = (char*) malloc(len * sizeof(char));
            strcpy(data, table->data[index]);
            return 1;
        }
    }

    return 0;
}

void print_list(list* list) {
    node* current = list->start;
    int first = 1;

    (current != NULL) ? printf("[") : printf("");
    while (current != NULL) {
        (first) ? printf("%s", current->data) : printf(", %s", current->data);
        first = 0;
        current = current->next;
    }
    (first == 0) ? printf("]\n") : printf("");
}

void print_chaining_table(chaining_table* table) {
    for (int i = 0; i < table->size; i += 1) {
        print_list(table->data_lists[i]);
    }
}



int main(void) {
    chaining_table* table_c = create_chaining_table(SIZE, ascii_hash);

    int output;

    if ((output = add_ct("abc", table_c)) == 0) {
        fprintf(stderr, "[ERROR in main()]: Failed to add abc to chaining table");
    }
    if ((output = add_ct("sam", table_c)) == 0) {
        fprintf(stderr, "[ERROR in main()]: Failed to add sam to chaining table");
    }
    if ((output = add_ct("int", table_c)) == 0) {
        fprintf(stderr, "[ERROR in main()]: Failed to add int to chaining table");
    }
    if ((output = add_ct("bca", table_c)) == 0) {  // Collides with "abc"
        fprintf(stderr, "[ERROR in main()]: Failed to add bca to chaining table");
    }
    
    print_chaining_table(table_c);

    if ((output = remove_ct("int", table_c)) == 0) {
        fprintf(stderr, "[ERROR in main()]: Failed to remove int from chaining table");
    }

    print_chaining_table(table_c);

    free_chaining_table(table_c);

    return 0;
}
