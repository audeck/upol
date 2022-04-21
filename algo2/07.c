#include <stdio.h>
#include <stdlib.h>

#define T 3

typedef struct {
    int keys[2 * T - 1];
    node* children[2 * T];
    node* parent;
    int n;
    char leaf;
} node;

typedef struct {
    node* root;
} b_tree;

typedef struct {
    node* node;
    int index;
} search_result;

typedef struct {
    int median;
    node* left;
    node* right;
} split_result;

split_result* create_split_result(int median, node* left, node* right) {
    split_result* result = (split_result*) malloc(sizeof(split_result));

    result->median = median;
    result->left = left;
    result->right = right;

    return result;
}

search_result* search(node* root, int value) {
    int i = 0;

    while (i < root->n && value > root->keys[i]) {
        i += 1;
    }

    if (i < root->n && value == root->keys[i]) {
        search_result* result = (search_result*) malloc(sizeof(search_result));
        result->node = root;
        result->index = i;
        return result;
    }
    else if (root->leaf) {
        return NULL;
    }
    else {
        return search(root->children[i], value);
    }
}

node* find_insertion_node(node* root, int value) {
    int i = 0;

    while (i < root->n && value > root->keys[i]) {
        i += 1;
    }

    if (root->leaf) {
        return root;
    }
    else {
        return find_insertion_node(root->children[i], value);
    }
}

/* Shift target's children and keys right to make space at index */
void shift_right(node* target, int index) {
    node** children = target->children;
    int*   keys     = target->keys;
    int    n        = target->n;

    children[n] = children[n - 1];

    for (int i = n - 1; i > index; i -= 1) {
        keys[i] = keys[i - 1];
        children[i] = children[i - 1];
    }
}

/* Inserts child into parent's children at index */
void insert_child(node* parent, int index, node* child) {
    parent->children[index] = child;
    if (child != NULL) child->parent = parent;
}

/* Splits a full node into two nodes (subtrees) and returns pointer to struct containing (median, left, right) */
split_result* split_node(node* target) {
    int median = target->keys[T - 1];
    target->n = T - 1;

    /* Create new right "subtree" (target becomes the left one) */
    node* right = (node*) malloc(sizeof(node));
    right->leaf = target->leaf;
    right->n = T - 1;

    /* Copy keys and children */
    for (int i = 0; i < T - 1; i += 1) {
        right->keys[i] = target->keys[T + i];
        right->children[i] = target->children[T + i];
    }
    right->children[T - 1] = target->children[2 * T - 1];

    return create_split_result(median, target, right);
}

void insert_with_subtrees(b_tree* tree, node* target, int data, node* left, node* right) {
    /* If root got split */
    if (target == NULL) {
        node* new_root = (node*) malloc(sizeof(node));
        new_root->keys[0] = data;
        new_root->leaf = 0;
        new_root->n = 1;
        new_root->parent = NULL;

        insert_child(new_root, 0, left);
        insert_child(new_root, 1, right);

        tree->root = new_root;
    }
    /* If target node is full */
    else if (target->n == 2 * T - 1) {
        node* parent = target->parent;

        split_result* split = split_node(target);
        int   parent_key    = split->median;
        node* l             = split->left;
        node* r             = split->right;

        if (data < parent_key) {
            insert_with_subtrees(tree, l, data, left, right);
        }
        else {
            insert_with_subtrees(tree, r, data, left, right);
        }

        insert_with_subtrees(tree, parent, parent_key, l, r);
    }
    /* If target isn't full */
    else {
        /* Find insertion index */
        int i = 0;
        while (i < target->n && data > target->keys[i]) i += 1;

        /* Make space for data */
        shift_right(target, i);

        /* Insert data */
        target->keys[i] = data;
        target->n += 1;

        /* Insert children */
        insert_child(target, i, left);
        insert_child(target, i + 1, right);
    }
}

void insert(b_tree* tree, int data) {
    node* target = find_insertion_node(tree->root, data);
    insert_with_subtrees(tree, target, data, NULL, NULL);
}
