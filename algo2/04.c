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
    return 1 + max(_depth(t.root->left), _depth(t.root->right));
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

}

void print_bft(tree *t) {
    
}



int main(void) {
    /* TODO: Tests */
    return 0;
}