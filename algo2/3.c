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
    node* top;
} stack;



int _length(node* e) {
    if (e->next != NULL) {
        return (1 + _length(e->next));
    } else {
        return 1;
    }
}

int length(list seznam) {
    return _length(seznam.first);
}

void _print_list(node* e) {
    if (e->next != NULL) {
        printf("%i, ", e->data);
        _print_list(e->next);
    } else {
        printf("%i\n", e->data);
    }
}

void print_list(list seznam) {
    _print_list(seznam.first);
}

void add_start(list* seznam, node* uzel) {
    node* second = seznam->first;  // Can be NULL
    uzel->next = second;
    seznam->first = uzel;
}

void _add_end(node* e, node* uzel) {
    if (e->next != NULL) {
        _add_end(e->next, uzel);
    } else {
        e->next = uzel;
    }
}

void add_end(list* seznam, node* uzel) {
    if (seznam->first == NULL) {
        seznam->first = uzel;
    } else {
        _add_end(seznam->first, uzel);
    }
}

void _add_position(node* e, node* uzel, int position) {
    if (e->next == NULL || position <= 0) {
        e->next = uzel;
    } else {
        _add_position(e, uzel, position - 1);
    }
}

void add_position(list* seznam, node* uzel, int position) {
    if (seznam->first == NULL) {
        seznam->first = uzel;
    } else {
        _add_position(seznam->first, uzel, position);
    }
}

int remove_start(list* seznam) {
    if (seznam->first != NULL) {
        node* new_first = seznam->first->next;
        free(seznam->first);
        seznam->first = new_first;
        return 0;
    } else {
        return -1;
    }
}

int _remove_end(node* e) {
    if (e->next->next == NULL) {
        free(e->next);
        e->next = NULL;
    }
}

int remove_end(list* seznam) {
    switch (length(*seznam)) {
        case 0:
            return -1;
        case 1:
            free(seznam->first);
            seznam->first = NULL;
            return 0;
        default:
            _remove_end(seznam->first);
    }
}

int _search(node* e, node* uzel) {
    if (e->data == uzel->data) {
        return 0;
    } else if (e->next == NULL) {
        return -1;
    } else {
        return (1 + _search(e->next, uzel));
    }
}

int search(list* seznam, node* uzel) {
    if (seznam->first != NULL) {
        return _search(seznam->first, uzel);
    } else {
        return -1;
    }
}

int _remove(node* e, node* uzel) {
    if (e->next->data == uzel->data) {
        node* new_next = e->next->next;
        free(e->next);
        e->next = new_next;
        return 0;
    } else if (e->next->next == NULL) {
        return -1;
    } else {
        return _remove(e->next, uzel);
    }
}

int remove(list* seznam, node* uzel) {
    /* If 'seznam' is of length = 1 */
    if (seznam->first != NULL && seznam->first->next == NULL && seznam->first->data == uzel->data) {
        free(seznam->first);
        seznam->first = NULL;
        return 0;
    /* If 'seznam' is of length > 1 */
    } else if (seznam->first->next != NULL) {
        return _remove(seznam->first, uzel);
    }

    /* If 'seznam' is of length = 0 or of length = 1 and the node != 'uzel' */
    return -1;
}



int main(void) {
    /* TODO: Tests */
}
