#include <stdio.h>
#include <stdlib.h>

typedef struct node {
    int data;
    char bf;  // Balance factor
    struct node* left;
    struct node* right;
    struct node* parent;
} node;

node* create_node(int data) {
    node* ptr = (node*) malloc(sizeof(node));

    ptr->data = data;
    ptr->bf = 0;
    ptr->left = NULL;
    ptr->right = NULL;
    ptr->parent = NULL;
}

typedef struct tree {
    node* root;
} tree;

tree* create_tree() {
    tree* ptr = (tree*) malloc(sizeof(tree));

    if (ptr = NULL) {
        fprintf(stderr, "[ERROR in create_tree()]: Failed to allocate tree");
        return NULL;
    }

    ptr->root = NULL;

    return ptr;
}


void rotate_right(node* root) {
    node* prev_root = root;
    node* new_root = root->left;
    node* root_parent = root->parent;

    /* Update parenting */
    new_root->parent = prev_root->parent;
    prev_root->parent = new_root;
    if (root_parent != NULL) {
        if (root_parent->left == prev_root) {
            root_parent->left = new_root;
        } else {
            root_parent->right = new_root;
        }
    }

    /* Rearrange children */
    prev_root->left = new_root->right;
    new_root->right = prev_root;

    /* Update balance factors */
    prev_root->bf = 1 - new_root->bf;
    new_root->bf -= 1;
}

void rotate_left(node* root) {
    node* prev_root = root;
    node* new_root = root->right;
    node* root_parent = root->parent;

    /* Update parenting */
    new_root->parent = prev_root->parent;
    prev_root->parent = new_root;
    if (root_parent != NULL) {
        if (root_parent->left == prev_root) {
            root_parent->left = new_root;
        } else {
            root_parent->right = new_root;
        }
    }

    /* Rearrange children */
    prev_root->right = new_root->left;
    new_root->left = prev_root;

    /* Update balance factors */
    prev_root->bf = new_root->bf - 1;
    new_root->bf += 1;
}

void add_node(tree* t, int data) {
    node* new = create_node(data);

    node* y = NULL;
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
    x = new;

    if (y == NULL) {
        t->root = new;
    } else if (new->data < y->data) {
        y->left = new;
    } else {
        y->right = new;
    }

    /* Go through the tree bottom-up & correct balance factors ('y' is always the parent of 'x') */
    while (y != NULL) {
        y->bf += 1 - (2 * (x == y->left));

        switch (y->bf) {
            case 0:
                return;

            case 2:
                if (y->left >= 0) {
                    rotate_right(y);
                } else {
                    rotate_left(y->left);
                    rotate_right(y);
                }
                break;

            /* Should be case 2 mirrored */
            case -2:
                if (y->right >= 0) {
                    rotate_left(y);
                } else {
                    rotate_right(y->right);
                    rotate_left(y);
                }
                break;
        }

        /* Shift 'x' and 'y' upwards */
        x = y;
        y = y->parent;
    }
}



int main(void) {
    tree* AVL = create_tree();
    add_node(AVL, 1);
    add_node(AVL, 2);
    add_node(AVL, 3);
    add_node(AVL, 4);
    add_node(AVL, 5);

    return 0;
}
