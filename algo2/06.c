
/* All trees and tree usage in this file are AVL (bst-)trees */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define max(a, b) ((a > b) ? a : b)

/* ~~~ Structs and methods ~~~ */

typedef struct node {
    int data;
    char bf;  // Balance factor
    struct node* left;
    struct node* right;
    struct node* parent;
} node;

node* create_node(int data) {
    node* ptr = (node*) malloc(sizeof(node));

    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate node");
        return NULL;
    }

    ptr->data = data;
    ptr->bf = 0;
    ptr->left = NULL;
    ptr->right = NULL;
    ptr->parent = NULL;

    return ptr;
}

typedef struct tree {
    node* root;
} tree;

tree* create_tree() {
    tree* ptr = (tree*) malloc(sizeof(tree));

    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_avl_tree()]: Failed to allocate avl_tree");
        return NULL;
    }

    ptr->root = NULL;

    return ptr;
}

void _free_all_nodes(node* node) {
    if (node != NULL) {
        _free_all_nodes(node->left);
        _free_all_nodes(node->right);
        free(node);
    }
}

void free_tree(tree* tree) {
    _free_all_nodes(tree->root);
    free(tree);
}

/* ~~~ Functions ~~~ */

/* Rotates an AVL tree "to the right" (x.left = y -> y.right = x) */
void rotate_right(node* parent) {
    node* child = parent->left;
    node* grand_parent = parent->parent;

    /* Update parenting */
    child->parent = grand_parent;
    parent->parent = child;

    if (grand_parent != NULL) {
        (grand_parent->left == parent) ? (grand_parent->left = child) : (grand_parent->right = child);
    }

    /* Rearrange children */
    parent->left = child->right;
    if (parent->left != NULL) parent->left->parent = parent;
    child->right = parent;

    /* Update balance factors */
    parent->bf -= (1 + child->bf);
    child->bf -= 1;
}

/* Rotates an AVL tree "to the left" (x.right = y -> y.left = x) */
void rotate_left(node* parent) {
    node* child = parent->right;
    node* grand_parent = parent->parent;

    /* Update parenting */
    child->parent = grand_parent;
    parent->parent = child;

    if (grand_parent != NULL) {
        (grand_parent->left == parent) ? (grand_parent->left = child) : (grand_parent->right = child);
    }

    /* Rearrange children (& update parenting!) */
    parent->right = child->left;
    if (parent->right != NULL) parent->right->parent = parent;
    child->left = parent;

    /* Update balance factors */
    parent->bf += (1 - child->bf);
    child->bf += 1;
}

/* Adds node (containing data) to tree and returns it's pointer */
node* add_node(tree* tree, int data) {
    node* new = create_node(data);

    node* y = NULL;
    node* x = tree->root;

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
        tree->root = new;
    } else  {
        (new->data < y->data) ? (y->left = new) : (y->right = new);
    }

    return new;
}

/* "Balances" out an AVL tree using balance factors from start up */
void balance(tree* tree, node* start) {
    node* u = start->parent;
    node* v = start;  // u is always parent of v
    node* p;  // Used to update root if needed

    while (u != NULL) {
        /* Update balance factor */
        (u->left == v) ? (u->bf += 1) : (u->bf -= 1);

        /* Rotations */
        if (u->bf == -2) {
            p = u->parent;
            if (v->bf == 1) rotate_right(v);
            rotate_left(u);
            if (p == NULL) tree->root = u->parent;
        }
        if (u->bf == 2) {
            p = u->parent;
            if (v->bf == -1) rotate_left(v);
            rotate_right(u);
            if (p == NULL) tree->root = u->parent;
        } 

        if (u->bf == 0) return;

        /* Move one level above */
        v = u;
        u = u->parent;
    }
}

/* Adds node w/ data to AVL tree and balances it's structure out */
void add_node_balanced(tree* tree, int data) {
    node* added = add_node(tree, data);
    balance(tree, added);
}

/* PRIVATE */
int _depth(node* node) {
    if (node == NULL) return 0;
    return 1 + max(_depth(node->left), _depth(node->right));
}

/* Returns the depth of a tree */
int depth(tree* tree) {
    return _depth(tree->root);
}

/* PRIVATE */
void _print_digraph(node* node) {
    if (node == NULL) return;

    _print_digraph(node->left);
    if (node->parent != NULL) printf("%i -> ", node->parent->data);
    printf("%i;\n", node->data);
    _print_digraph(node->right);
}

/* Prints out a digraph (tree in dot format) visualizable @ http://www.webgraphviz.com */
void print_digraph(tree* tree) {
    if (tree == NULL) return;

    printf("digraph {\n");
    _print_digraph(tree->root);
    printf("}\n");
}

/* ~~~ MAIN ~~~ */

int main(void) {
    // tree* AVL = create_tree();

    // add_node_balanced(AVL, 1);
    // add_node_balanced(AVL, 2);
    // add_node_balanced(AVL, 3);
    // add_node_balanced(AVL, 4);
    // add_node_balanced(AVL, 5);
    // add_node_balanced(AVL, 6);
    // add_node_balanced(AVL, 7);

    // print_digraph(AVL);

    // free_tree(AVL);

    return 0;
}
