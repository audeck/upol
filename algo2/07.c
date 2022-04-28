#include <stdio.h>
#include <stdlib.h>

#define T 3

typedef struct {
    int keys[2 * T - 1];
    node* children[2 * T];
    node* parent;
    int n;  // Number of keys
    char leaf;
} node;

typedef struct {
    node* root;
} b_tree;

typedef struct {
    node* node;
    int index;
} search_result;

search_result* create_search_result(node* found_node, int index) {
    search_result* result = (search_result*) malloc(sizeof(search_result));

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
        return create_search_result(root, i);
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

/* Splits a full node into two nodes (subtrees) and returns pointer to struct containing [median, left, right] */
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

void shift_left(node* target, int index) {
    node** children = target->children;
    int* keys       = target->keys;

    for (int i = index; i < target->n - 1; i += 1) {
        keys[i] = keys[i + 1];
        children[i] = children[i + 1];  // Might not work for leaves?
    }
}

int find_child_index(node* child) {
    if (child->parent == NULL) return -1;

    int i = 0;
    node* parent = child->parent;

    while (parent->children[i] != child) i += 1;

    return i;
}

void copy_end(node* recipient, node* donor, int merger) {
    int r_n = recipient->n;
    int d_n = donor->n;

    recipient->keys[r_n] = merger;
    r_n += 1;

    for (int i = 0; i < d_n; i += 1) {
        recipient->keys[r_n + i] = donor->keys[i];
        recipient->children[r_n + i] = donor->children[i];
    }

    recipient->children[r_n + d_n + 1] = donor->children[d_n];

    recipient->n = r_n + d_n + 1;
}

void delete(b_tree* tree, int data) {
    search_result* searc = search(tree->root, data);
    node* target = searc->node;
    int index = searc->index;

    if (target->leaf) {
        shift_left(target, index);
        target->n -= 1;
    }
    else {
        int new_key = target->children[index + 1][0];
        delete(tree, new_key);
        target->keys[index] = new_key;
    }

    while (target != tree->root && target->n < T - 1) {
        int target_index = find_child_index(target);
        node* parent = target->parent;
        node* left_sibling = (target_index > 0) ? parent->children[target_index - 1] : NULL;
        node* right_sibling = (target_index < parent->n) ? parent->children[target_index + 1] : NULL;  // parent->n - 1? - NO(?)

        if (left_sibling != NULL && left_sibling->n < T - 1) {
            // Transfer key right
            int n = left_sibling->n;

            shift_right(target, 0);
            target->keys[0] = parent->keys[target_index - 1];
            target->children[0] = left_sibling->children[n];
            target->n += 1;

            parent->keys[target_index - 1] = left_sibling->keys[n - 1];
            left_sibling->n -= 1;
        }
        else if (right_sibling != NULL && right_sibling->n < T - 1) {
            // Transfer key left
            int n = target->n;

            target->keys[n] = parent->keys[target_index];
            target->children[n + 1] = right_sibling->children[0];

            parent->keys[target_index] = right_sibling->keys[0];

            shift_left(right_sibling, 0);
            right_sibling->n -= 1;
        }
        else {
            // Merge target with right sibling (both siblings are of n >= T - 1)
            copy_end(target, right_sibling, parent->keys[target_index]);
            shift_left(parent, target_index);
            parent->children[target_index] = target;
            free(right_sibling);

            /* Update tree->root if needed */
            if (parent == tree->root && parent->n == 0) {
                tree->root = target;
            }
        }
    }
}



int main(void) {
    return 0;
}
