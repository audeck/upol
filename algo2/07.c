
/* All trees and tree usage in this file are B trees */

#include <stdio.h>
#include <stdlib.h>

#define T 3

/* ~~~ Structs and methods ~~~ */

typedef struct node {
    int* keys;  // [2 * T - 1]
    struct node** children; // [2 * T];
    struct node* parent;
    int n;  // Number of keys
    char leaf;
} node;

node* create_node() {
    node* ptr = (node*) malloc(sizeof(node));

    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate node");
        return NULL;
    }

    ptr->n = 0;
    ptr->leaf = 0;
    ptr->parent = NULL;
    ptr->keys = (int*) malloc((2 * T - 1) * sizeof(int));
    ptr->children = (node**) malloc((2 * T) * sizeof(node*));

    if (ptr->keys == NULL || ptr->children == NULL) {
        fprintf(stderr, "[ERROR in create_node()]: Failed to allocate node's fields");
        free(ptr->children);
        free(ptr->keys);
        free(ptr);
        return NULL;
    }

    return ptr;
}

void free_node(node* node) {
    free(node->children);
    free(node->keys);
    free(node);
}

typedef struct {
    node* root;
} tree;

tree* create_tree() {
    tree* ptr = (tree*) malloc(sizeof(tree));

    if (ptr == NULL) {
        fprintf(stderr, "[ERROR in create_tree()]: Failed to allocate tree");
        return NULL;
    }

    ptr->root = NULL;

    return ptr;
}

void _free_all_nodes(node* node) {
    if (node != NULL) {
        for (int i = 0; i < node->n; i += 1) _free_all_nodes(node->children[i]);
        free_node(node);
    }
}

void free_tree(tree* tree) {
    _free_all_nodes(tree->root);
    free(tree);
}

typedef struct {
    node* node;
    int index;
} search_result;

search_result* create_search_result(node* found_node, int index) {
    search_result* result = (search_result*) malloc(sizeof(search_result));

    if (result == NULL) {
        fprintf(stderr, "[ERROR in create_search_result()]: Failed to allocate result");
        return NULL;
    }

    result->node = found_node;
    result->index = index;

    return result;
}

typedef struct {
    int median;
    node* left;
    node* right;
} split_result;

split_result* create_split_result(int median, node* left, node* right) {
    split_result* result = (split_result*) malloc(sizeof(split_result));

    if (result == NULL) {
        fprintf(stderr, "[ERROR in create_split_result()]: Failed to allocate result");
        return NULL;
    }

    result->median = median;
    result->left = left;
    result->right = right;

    return result;
}

/* ~~~ Functions ~~~ */

/*  
    Tries to find value in tree (given by it's root node) and returns 
    pointer to a struct {found_node, index_of_value} if found, NULL
    if not found
*/
search_result* search(node* root, int value) {
    if (root == NULL) return NULL;

    /* Try to find value's index in root */
    int i = 0;
    while (i < root->n && value > root->keys[i]) i += 1;

    /* Key was found */
    if (i < root->n && value == root->keys[i]) return create_search_result(root, i);

    /* Key wasn't found and root doesn't have children */
    if (root->leaf) return NULL;

    /* Key wasn't found -> search root's (correct) child */
    return search(root->children[i], value);
}

/* Returns the node into which value should be inserted */
node* find_insertion_node(node* root, int value) {
    if (root == NULL) return NULL;

    int i = 0;
    while (i < root->n && value > root->keys[i]) i += 1;

    if (root->leaf) return root;
    return find_insertion_node(root->children[i], value);
}

/* Shift target's children and keys right to make space at index (for insertion) */
void shift_right(node* target, int index) {
    node** children = target->children;
    int* keys = target->keys;
    int n = target->n;

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

/* Splits a full node into two nodes and returns pointer to a struct {median, left, right} */
split_result* split_node(node* target) {
    int median = target->keys[T - 1];
    target->n = T - 1;

    /* Create new right "subtree" (target becomes the left one) */
    node* right = create_node();
    right->parent = target->parent;
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

void insert_with_subtrees(tree* tree, node* target, int data, node* left, node* right) {
    /* If root got split or tree is empty */
    if (target == NULL) {
        node* new_root = create_node();
        new_root->keys[0] = data;
        new_root->leaf = (tree->root == NULL) ? 1 : 0;
        new_root->n = 1;

        insert_child(new_root, 0, left);
        insert_child(new_root, 1, right);

        tree->root = new_root;
    }
    /* If target node is full */
    else if (target->n == 2 * T - 1) {
        node* parent = target->parent;

        /* Split node and get result */
        split_result* split = split_node(target);
        int parent_key = split->median;
        node* l = split->left;
        node* r = split->right;
        free(split);

        /* Insert data into correct subtree */
        node* data_target = (data < parent_key) ? l : r;
        insert_with_subtrees(tree, data_target, data, left, right);

        /* Insert split median and l, r into parent */
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

void insert(tree* tree, int data) {
    node* target = find_insertion_node(tree->root, data);
    insert_with_subtrees(tree, target, data, NULL, NULL);
}

void _print_node_data(node* node) {
    char first = 1;

    printf("\"");
    for (int i = 0; i < node->n; i += 1) {
        if (!first) printf(",");
        printf("%i", node->keys[i]);
        first = 0;
    }
    printf("\"");
}

void _print_digraph(node* node) {
    if (node == NULL) return;

    if (node->parent != NULL) {
        _print_node_data(node->parent);
        printf(" -> ");
    }

    _print_node_data(node);
    printf(";\n");

    if (!node->leaf) {
        for (int i = 0; i <= node->n; i += 1) {
            _print_digraph(node->children[i]);
        }
    }
}

void print_digraph(tree* tree) {
    if (tree == NULL) return;

    printf("digraph {\n");
    _print_digraph(tree->root);
    printf("}\n");
}

/* ~~~ MAIN ~~~ */

int main(void) {
    // tree* b = create_tree();

    // insert(b, 0);
    // insert(b, 1);
    // insert(b, 2);
    // insert(b, 3);
    // insert(b, 4);
    // print_digraph(b);
    // insert(b, 5);
    // print_digraph(b);
    // insert(b, 6);
    // insert(b, 7);
    // insert(b, 8);
    // insert(b, 9);
    // insert(b, 10);
    // insert(b, 11);
    // print_digraph(b);

    // free_tree(b);

    return 0;
}
