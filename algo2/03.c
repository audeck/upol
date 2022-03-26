#include <stdio.h>
#include <stdlib.h>

/* ~~~ Types & constructors (+ destructors(?)) ~~~ */

typedef struct node {
    int data;
    struct node* next;
} node;

/* Allocates a node and returns it's pointer */
node* create_node(int data) {
    node* ptr = (node*) malloc(sizeof(node));
    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate!");
        return NULL;
    }

    ptr->data = data;
    ptr->next = NULL;
    return ptr;
}

typedef struct list {
    node* first;
} list;

/* Allocates a list and returns it's pointer */
list* create_list() {
    list* ptr = (list*) malloc(sizeof(list));
    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_list()]: Failed to allocate!");
        return NULL;
    }

    ptr->first = NULL;
    return ptr;
}

/* Deallocates a list and all it's nodes */
void free_list(list* list) {
    node* current_node = list->first;
    node* next_node;

    while (current_node != NULL) {
        next_node = current_node->next;
        free(current_node);
        current_node = next_node;
    }

    free(list);
}

typedef struct stack {
    node* last;
} stack;

/* Allocates a stack and returns it's pointer */
stack* create_stack() {
    stack* ptr = (stack*) malloc(sizeof(stack));
    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_stack()]: Failed to allocate!");
        return NULL;
    }

    ptr->last = NULL;
    return ptr;
}

/* Deallocates a stack and all it's nodes */
void free_stack(stack* stack) {
    node* current_node = stack->last;
    node* next_node;

    while (current_node != NULL) {
        next_node = current_node->next;
        free(current_node);
        current_node = next_node;
    }

    free(stack);
}

typedef struct queue {
    node* first;
} queue;

/* Allocates a queue and returns it's pointer */
queue* create_queue() {
    queue* ptr = (queue*) malloc(sizeof(queue));
    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_queue()]: Failed to allocate!");
        return NULL;
    }

    ptr->first = NULL;
    return ptr;
}

/* Deallocates a queue and all it's nodes */
void free_queue(queue* queue) {
    node* current_node = queue->first;
    node* next_node;

    while (current_node != NULL) {
        next_node = current_node->next;
        free(current_node);
        current_node = next_node;
    }

    free(queue);
}

/* ~~~ Functions ~~~ */

/* Returns the length of a linked list */
int length(list seznam) {
    /* Get first node */
    node* current_node = seznam.first;
    int len = 0;

    /* "Walk" through list and increment 'len' */
    while (current_node != NULL) {
        current_node = current_node->next;
        len += 1;
    }

    return len;
}

/* Prints out a linked list */
void print_list(list seznam) {
    /* Get first node */
    node* current_node = seznam.first;

    /* "Walk" through list and print 'data' */
    while (current_node != NULL) {
        printf("%i ", current_node->data);
        current_node = current_node->next;
    }

    printf("\n");
}

/* Adds a linked list node with 'data' to the start of 'seznam' */
void add_start(list* seznam, int data) {
    /* Create a new node and insert it */
    node* new = create_node(data);
    new->next = seznam->first;
    seznam->first = new;
}

/* Adds a linked list node with 'data' to the end of 'seznam' */
void add_end(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;
    node* prev_node;

    /* "Walk" through list */
    while (current_node != NULL) {
        prev_node = current_node;
        current_node = current_node->next;
    }

    /* Create new node */
    prev_node->next = create_node(data);
}

/* 'position' is the new node's index (?) */
void add_position(list* seznam, int data, int position) {
    /* Get first node */
    node* current_node = seznam->first;
    node* prev_node;

    /* Create a new node */
    node* new = create_node(data);

    /* If 'seznam' is empty or position is 0 */
    if (current_node == NULL || position == 0) {
        seznam->first = new;
        new->next = current_node;
        return;
    }

    /* "Walk" through list and decrement position */
    while (current_node != NULL && position > 0) {
        prev_node = current_node;
        current_node = current_node->next;
        position -= 1;
    }

    /* Insert new node */
    new->next = current_node;
    prev_node->next = new;
}

/* Removes the first linked list node from 'seznam' */
int remove_start(list* seznam) {
    /* Get first node */
    node* current_node = seznam->first;

    /* If 'seznam' is already empty */
    if (current_node == NULL) return -1;

    /* Remove first node & free up its space */
    seznam->first = current_node->next;
    free(current_node);
    return 0;
}

/* Removes the last linked list node from 'seznam' */
int remove_end(list* seznam) {
    node* current_node = seznam->first;
    node* prev_node;

    /* If list is empty */
    if (current_node == NULL) return -1;

    /* If list's length is 1 */
    if (current_node->next == NULL) {
        seznam->first == NULL;
        free(current_node);
        return 0;
    }
    
    /* Default case */
    while (current_node->next != NULL) {
        prev_node = current_node;
        current_node = current_node->next;
    }

    prev_node->next = NULL;
    free(current_node);

    return 0;
}

/* Returns the index of the first linked list node with data of 'data' in 'seznam' */
int search(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;
    int pos = 0;

    /* "Walk" through list */
    while (current_node != NULL) {
        /* Compare node's value to 'data' & return 'pos' (node's index in 'seznam') if true */
        if (current_node->data == data) return pos;

        /* Otherwise go to next node & increment 'pos' */
        current_node = current_node->next;
        pos += 1;
    }

    /* Return reachable only if 'data' isn't in 'seznam' (hopefully) */
    return -1;
}

/* Removes the first linked list node with data of 'data' from 'seznam' (NOTE: "remove()" is declared in stdio.h) */
int list_remove(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;
    node* prev_node;

    /* Check if 'seznam' is empty */
    if (current_node == NULL) return -1;

    /* Check if first node matches 'data' (if length of 'seznam' is 1) */
    if (current_node->data == data) {
        seznam->first = current_node->next;
        free(current_node);
        return 0;
    }

    /* Go to next node */
    prev_node = current_node;
    current_node = current_node->next;

    /* "Walk" through list */
    while (current_node != NULL) {
        /* Remove & free node if 'data' matches node's data */
        if (current_node->data == data) {
            prev_node->next = current_node->next;
            free(current_node);
            return 0;
        }

        /* Otherwise just go to next node */
        prev_node = current_node;
        current_node = current_node->next;
    }

    /* Return reachable only if 'data' isn't in 'seznam' (hopefully) */
    return -1;
}

/* ~~~ Stack functionality ~~~ */

/* Pushes a new node with data 'data' */
void push(stack* zasobnik, int data) {
    node* new = create_node(data);

    if (zasobnik->last == NULL) {
        zasobnik->last = new;
    } else {
        new->next = zasobnik->last;
        zasobnik->last = new;
    }
}

/* Pops a node and returns it's data */
int pop(stack* zasobnik) {
    node *popped = zasobnik->last;

    if (popped != NULL) {
        zasobnik->last = popped->next;
    }

    return popped->data;
}

/* ~~~ Queue functionality ~~~ */

/* Enqueues a new node with data 'data' */
void enqueue(queue* fronta, int data) {
    node* new = create_node(data);

    /* Get first node */
    node* current_node = fronta->first;
    node* prev_node;

    /* If queue is empty */
    if (current_node == NULL) {
        fronta->first = new;
        return;
    }

    prev_node = current_node;
    current_node = current_node->next;

    /* "Walk" through list */
    while (current_node != NULL) {
        prev_node = current_node;
        current_node = current_node->next;
    }

    /* Add node */
    prev_node->next = new;
}

/* Dequeues a node and returns it's data */
int dequeue(queue* fronta) {
    node *dequeued = fronta->first;

    if (dequeued != NULL) {
        fronta->first = dequeued->next;
    }

    return dequeued->data;
}

/* ~~~ MAIN ~~~ */

int main(void) {
    // list* linked_list = create_list();

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // add_start(linked_list, 3);
    // add_start(linked_list, 6);

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // add_end(linked_list, 5);

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // add_position(linked_list, 9, 2);

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // remove_start(linked_list);
    // remove_end(linked_list);

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // int to_search = 9;
    // printf("%i is at index %i.\n", to_search, search(linked_list, to_search));

    // int first_remove = list_remove(linked_list, 9);
    // int second_remove = list_remove(linked_list, 9);

    // if (first_remove != 0 || second_remove != -1) {
    //     printf("Something went wrong while removing #1!\n");
    // }

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // list_remove(linked_list, 3);
    // int start = remove_start(linked_list);
    // int end = remove_end(linked_list);

    // if (start != -1 || end != -1) {
    //     printf("Something went wrong while removing #2!\n");
    // }

    // printf("List (of length %i): ", length(*linked_list));
    // print_list(*linked_list);

    // free_list(linked_list);

    // stack* stack = create_stack();
    // push(stack, 1);
    // push(stack, 2);
    // push(stack, 3);

    // printf("Stack pops: ");
    // printf("%i ", pop(stack));
    // printf("%i ", pop(stack));
    // printf("%i ", pop(stack));
    // printf("\n");

    // free_stack(stack);

    // queue* queue = create_queue();
    // enqueue(queue, 1);
    // enqueue(queue, 2);
    // enqueue(queue, 3);

    // printf("Queue dequeues: ");
    // printf("%i ", dequeue(queue));
    // printf("%i ", dequeue(queue));
    // printf("%i ", dequeue(queue));
    // printf("\n");

    // free_queue(queue);

    return 0;
}
