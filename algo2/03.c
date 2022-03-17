#include <stdio.h>
#include <stdlib.h>

typedef struct node {
    int data;
    node* next;
} node;

typedef struct list {
    node* first;
} list;

typedef struct stack {
    node* last;
} stack;

typedef struct queue {
    node* first;
} queue;

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
        current_node = current_node->next;
        printf("%i,", current_node->data);
    }
}

/* Adds a linked list node with 'data' to the start of 'seznam' */
void add_start(list* seznam, int data) {
    /* Create a new node and insert it */
    node* new = malloc(sizeof(node));
    new->data = data;
    new->next = seznam->first;
    seznam->first = new;
}

/* Adds a linked list node with 'data' to the end of 'seznam' */
void add_end(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;

    /* "Walk" through list */
    while (current_node != NULL) {
        current_node = current_node->next;
    }

    /* Create new node */
    current_node->next = malloc(sizeof(node));
    current_node->next->data = data;
    current_node->next->next = NULL;
}

/* 'position' is the new node's index (?) */
void add_position(list* seznam, int data, int position) {
    /* Get first node */
    node* current_node = seznam->first;
    int pos = position - 1;

    /* Create a new node */
    node* new = malloc(sizeof(node));
    new->data = data;
    new->next = NULL;

    /* If 'seznam' is empty */
    if (current_node == NULL) {
        seznam->first = new;
        return;
    }

    /* "Walk" through list and decrement position */
    while (current_node != NULL && pos > 0) {
        current_node = current_node->next;
        pos -= 1;
    }

    /* Insert new node */
    new->next = current_node->next;
    current_node->next = new;
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
    /* Three distinguishable cases based on length of 'seznam': */
    switch (length(*seznam)) {
        /* Empty list case */
        case 0:
            return -1;

        /* One node list case */
        case 1:
            free(seznam->first);
            seznam->first = NULL;
            return 0;

        /* Two+ nodes list case */
        default:
            /* Get first node */
            node* current_node = seznam->first;

            /* "Walk" through list - ->next->next to ensure we end up on the second to last node */
            while (current_node->next->next != NULL) {
                current_node = current_node->next;
            }

            /* Remove last node & free up its space */
            free(current_node->next);
            current_node->next = NULL;
            return 0;
    }
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

/* Removes the first linked list node with data of 'data' from 'seznam' */
int remove(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;

    /* Check if 'seznam' is empty */
    if (current_node == NULL) return -1;

    /* Check if first node matches 'data' (for if length of 'seznam' is 1) */
    if (current_node->data == data) {
        seznam->first = current_node->next;
        free(current_node);
        return 0;
    }

    /* "Walk" through list */
    while (current_node->next != NULL) {
        /* Remove & free node if 'data' matches node's data */
        if (current_node->next->data == data) {
            node* old_node = current_node->next;
            current_node->next = current_node->next->next;
            free(old_node);
            return 0;
        }

        /* Otherwise go to next node */
        current_node = current_node->next;
    }

    /* Return reachable only if 'data' isn't in 'seznam' (hopefully) */
    return -1;
}

/* Removes all linked list nodes with data of 'data' from 'seznam' */
int removeAll(list* seznam, int data) {
    /* Get first node */
    node* current_node = seznam->first;
    int ret = -1;  // Return value

    /* Check if 'seznam' is empty */
    if (current_node == NULL) return ret;

    /* Check if first node matches 'data' (for if length of 'seznam' is 1) */
    if (current_node->data == data) {
        seznam->first = current_node->next;
        free(current_node);
        ret = 0;
    }

    /* "Walk" through list */
    while (current_node->next != NULL) {
        /* Remove & free node if 'data' matches node's data */
        if (current_node->next->data == data) {
            node* old_node = current_node->next;
            current_node->next = current_node->next->next;
            free(old_node);
            ret = 0;
        }

        /* Otherwise go to next node */
        current_node = current_node->next;
    }

    /* Return -1 if no removals or 0 if any removals*/
    return ret;
}

/* STACK */
void push(stack* zasobnik, node* data) {
    if (zasobnik->last == NULL) {
        zasobnik->last = data;
    } else {
        data->next = zasobnik->last;
        zasobnik->last = data;
    }
}

/* STACK */
node* pop(stack* zasobnik) {
    node *popped = zasobnik->last;

    if (popped != NULL) {
        zasobnik->last = popped->next;
    }

    return popped;
}

/* QUEUE */
void add_endQ(queue* fronta, node* data) {
    /* Get first node */
    node* current_node = fronta->first;

    /* "Walk" through list */
    while (current_node != NULL) {
        current_node = current_node->next;
    }

    /* Add node */
    current_node->next = data;
}

/* QUEUE */
void enqueue(queue* fronta, node* data) {
    if (fronta->first == NULL) {
        fronta->first = data;
    } else {
        /* O(1) if we store a "last" pointer as well the "first" one in 'fronta' */
        add_endQ(fronta, data);
    }
}

/* QUEUE */
node* dequeue(queue* fronta) {
    node *dequeued = fronta->first;

    if (dequeued != NULL) {
        fronta->first = dequeued->next;
    }

    return dequeued;
}



int main(void) {
    /* TODO: Tests */
    return 0;
}
