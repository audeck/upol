#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

typedef struct {
    int (*hash)(char*);
    list** data_lists;
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

    /* Assign hash */
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