#include <stdio.h>
#include <stdlib.h>

/* Tree & node usage in this file is limited to binary search trees */
typedef struct node {
    int data;
    node* left;
    node* right;
    node* parent;
} node;

typedef struct tree {
    node* root;
} tree;

typedef struct qnode {
    int data;
    node* prev;
    node* next;
} qnode;

typedef struct queue {
    qnode* first;
    qnode* last;
} queue;

void add(tree *t, int data) {
    node* new = malloc(sizeof(node));
    new->data = data;
    new->left = NULL;
    new->right = NULL;
    new->parent = NULL;

    node* y;
    node* x = t->root;

    while (x != NULL) {
        y = x;
        if (data < x->data) {
            x = x->left;
        } else {
            x = x->right;
        }
    }

    new->parent = y;

    if (y == NULL) {
        t->root = new;
    } else if (new->data < y->data) {
        y->left = new;
    } else {
        y->right = new;
    }
}

void _print_in_order(node* n) {
    if (n != NULL) {
        _print_in_order(n->left);
        printf("%i, ", n->data);
        _print_in_order(n->right);
    }
}

void print_in_order(tree t) {
    _print_in_order(t.root);
}

int _depth(node* n) {
    if (n == NULL) {
        return 0;
    } else {
        return 1 + max(_depth(n->left), _depth(n->right));
    }
}

int depth(tree t) {
    return _depth(t.root);
}

int tree_max(node *root) {
    node* ptr = root;

    while (ptr->right != NULL) {
        ptr = ptr->right;
    }

    return ptr->data;
}

int tree_min(node *root) {
    node* ptr = root;

    while (ptr->left != NULL) {
        ptr = ptr->left;
    }

    return ptr->data;
}

int tree_remove(tree *t, int data) {
    node* cur = t->root;  // root could be NULL?

    /* If tree is empty */
    if (cur == NULL) return 0;

    /* Find 'data' node */
    while (cur->data != data) {
        if (cur == NULL) return 0;
        if (cur->data > data) {
            cur = cur->left;
        } else {
            cur = cur->right;
        }
    }

    /* Case 1: 'cur' has no children */
    if (cur->left == NULL && cur->right == NULL) {
        /* If 'cur' is root */
        if (cur->parent == NULL) {
            t->root = NULL;
        } else {
            node* parent = cur->parent;

            if (parent->left == cur) {
                parent->left = NULL;
            } else {
                parent->right = NULL;
            }
        }

        free(cur);
        return 1;
    }

    /* Case 2a: 'cur' only has a right child */
    if (cur->left == NULL) {
        /* If 'cur' is root */
        if (cur->parent == NULL) {
            t->root = cur->right;
        } else {
            node* parent = cur->parent;

            if (parent->left == cur) {
                parent->left = cur->right;
            } else {
                parent->right = cur->right;
            }
        }

        free(cur);
        return 1;
    }

    /* Case 2b: 'cur' only has a left child */
    if (cur->right == NULL) {
        /* If 'cur' is root */
        if (cur->parent == NULL) {
            t->root = cur->left;
        } else {
            node* parent = cur->parent;

            if (parent->left == cur) {
                parent->left = cur->left;
            } else {
                parent->right = cur->left;
            }
        }

        free(cur);
        return 1;
    }

    /* Case 3: 'cur' has both children (in place for style points) */
    if (cur->right != NULL && cur->left != NULL) {
        int left_max = tree_max(cur->left);

        if (!tree_remove(t, left_max)) {
            fprintf(stderr, "Something went wrong!");
        }

        cur->data = left_max;
        return 1;
    }

    /* ??? */
    return 0;
}

void _bft_enqueue(node* n, queue* q) {
    if (n != NULL) {
        qnode* qnode = malloc(sizeof(qnode));
        qnode->data = n->data;
        qnode->prev = q->last;
        q->last = qnode;
    }
}

void print_bft(tree *t) {
    if (t->root != NULL) {
        queue* print_order = malloc(sizeof(queue));
        print_order->first = NULL;
        print_order->last = NULL;

        _bft_enqueue(t->root, print_order);
    }
}



int main(void) {
    /* TODO: Tests */
    return 0;
}
