#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* NO MALLOC SUCCESS CHECKS (OMEGALUL) */

#define max(a, b) ((a > b) ? a : b)


/* Tree & node usage in this file is limited to binary search trees */
typedef struct node {
    int data;
    struct node* left;
    struct node* right;
    struct node* parent;
} node;

/* Creates a node with data 'data' and returns it's pointer */
node* create_node(int data) {
    node* ptr = (node*) malloc(sizeof(node));
    ptr->data = data;
    ptr->left = NULL;
    ptr->right = NULL;
    ptr->parent = NULL;

    return ptr;
}


/* Glorified root wrapper */
typedef struct tree {
    node* root;
} tree;

/* Creates a tree and returns it's pointer */
tree* create_tree() {
    tree* ptr = (tree*) malloc(sizeof(tree));
    ptr->root = NULL;

    return ptr;
}

/* Private function used in 'free_tree()' */
void _free_nodes(node* node) {
    if (node != NULL) {
        _free_nodes(node->left);
        _free_nodes(node->right);
        free(node);
    }
}

/* Deallocates a tree and all it's nodes */
void free_tree(tree* tree) {
    free(tree);
}


/* A 'node' wrapper for usage with 'queue' */
typedef struct qnode {
    node* node;
    struct qnode* twrd_first;
    struct qnode* twrd_last;
} qnode;

/* Creates a qnode for node 'node' and returns it's pointer */
qnode* create_qnode(node* node) {
    qnode* ptr = (qnode*) malloc(sizeof(qnode));
    ptr->twrd_first = NULL;
    ptr->twrd_last = NULL;
    ptr->node = node;

    return ptr;
}


/* O(1) q&dq queue 8^) */
typedef struct queue {
    qnode* first;
    qnode* last;
} queue;

/* Creates a queue and returns it's pointer */
queue* create_queue() {
    queue* ptr = (queue*) malloc(sizeof(queue));
    ptr->first = NULL;
    ptr->last = NULL;

    return ptr;
}

/* Enqueues a new qnode with qnode->node = n */
void enqueue(queue* q, node* n) {
    /* Create qnode */
    qnode* qn = create_qnode(n);

    /* If queue is empty */
    if (q->first == NULL) {
        q->first = qn;
        q->last = qn;
    } else {
        qnode* prev_last = q->last;
        prev_last->twrd_last = qn;
        qn->twrd_first = prev_last;
        q->last = qn;
    }
}

/* Dequeues a qnode and returns qnode->node->data */
int dequeue(queue* q) {
    if (q->first != NULL) {
        qnode* popped = q->first;
        int popped_data = popped->node->data;

        q->first = popped->twrd_last;  // Don't have to NULL q->last with right enqueue
        
        free(popped);
        return popped_data;
    }

    fprintf(stderr, "[dequeue() (WARNING)] Tried to dequeue from an empty queue (returned 0)\n");
    return 0;
}

/* Dequeues a qnode with node == NULL (and doesn't return anything) */
void _dequeue_null(queue* q) {
    if (q->first == NULL) {
        fprintf(stderr, "[dequeue_null() (WARNING)] Tried to dequeue from an empty queue\n");
    }

    qnode* popped = q->first;
    q->first = popped->twrd_last;  // Don't have to NULL q->last with right enqueue
    free(popped);
}

/* ~~~~~ */

/* Private print_in_order() helper */
void _print_in_order(node* n) {
    if (n != NULL) {
        /* Print left_child -> parent -> right_child (children can be entire subtrees) */
        _print_in_order(n->left);
        printf("%i ", n->data);
        _print_in_order(n->right);
    }
}

/* Prints out the values in a tree ordered from lowest to highest to stdout */
void print_in_order(tree t) {
    _print_in_order(t.root);
    printf("\n");
}

/* Adds a new node containing data 'data' to binary search tree 't' */
void add(tree *t, int data) {
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

    if (y == NULL) {
        t->root = new;
    } else if (new->data < y->data) {
        y->left = new;
    } else {
        y->right = new;
    }
}

/* Private depth() helper */
int _depth(node* n) {
    if (n == NULL) {
        return 0;
    } else {
        return (1 + max(_depth(n->left), _depth(n->right)));
    }
}

/* Returns the depth of a (general) tree */
int depth(tree t) {
    return _depth(t.root);
}

/* Returns the maximum value in a binary search tree with root 'node' */
int tree_max(node *root) {
    node* ptr = root;

    while (ptr->right != NULL) {
        ptr = ptr->right;
    }

    return ptr->data;
}

/* Returns the minimum value in a binary search tree with root 'node' */
int tree_min(node *root) {
    node* ptr = root;

    while (ptr->left != NULL) {
        ptr = ptr->left;
    }

    return ptr->data;
}

/* Removes node containing 'data' from tree 't' */
int tree_remove(tree *t, int data) {
    /* If tree is empty */
    if (t == NULL || t->root == NULL) return 0;

    node* cur = t->root;  // Current node pointer

    /* Find 'data' node */
    while (cur != NULL && cur->data != data) {
        if (cur->data > data) {
            cur = cur->left;
        } else {
            cur = cur->right;
        }
    }

    /* Case 0: 'cur' wasn't found */
    if (cur == NULL) return 0;

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
                cur->right->parent = parent;
            } else {
                parent->right = cur->right;
                cur->right->parent = parent;
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
                cur->left->parent = parent;
            } else {
                parent->right = cur->left;
                cur->left->parent = parent;
            }
        }

        free(cur);
        return 1;
    }

    /* Case 3: 'cur' has both children (in place for style points) */
    if (cur->right != NULL && cur->left != NULL) {
        /* Find left sub-tree maximum */
        int left_max = tree_max(cur->left);

        /* Remove 'left_max' from tree & error check just in case */
        if (!tree_remove(t, left_max)) {
            fprintf(stderr, "[tree_remove() (EXCEPTION)] Something went terribly wrong\n");
        }

        /* Swap in 'left_max' to current node's data */
        cur->data = left_max;
        return 1;
    }

    /* ??? */
    fprintf(stderr, "[tree_remove() (EXCEPTION)] Reached (what should be) an unreachable return\n");
    return 0;
}

/* Prints tree 't' breadth-first to stdout */
void print_bft(tree *t) {
    if (t->root != NULL) {
        /* Create queue */
        queue* print_queue = create_queue();

        /* Enqueue root node */
        enqueue(print_queue, t->root);

        while (print_queue->first != NULL) {
            qnode* first = print_queue->first;

            /* Enqueue children */
            if (first->node->left != NULL) {
                enqueue(print_queue, first->node->left);
            }

            if (first->node->right != NULL) {
                enqueue(print_queue, first->node->right);
            }

            /* Dequeue and print 'first' */
            printf("%i ", dequeue(print_queue));
        }

        printf("\n");
    }
}

/* TODO? */
void root_insert(node* root, node* new_root) {
    if (root != NULL) {

    }
}

/* Prints 'string' to stdout 'times' times */
void print_times(char* string, int times) {
    for (int i = 0; i < times; i += 1) {
        printf("%s", string);
    }
}

/* Prints BST(!) 't' to stdout in a structured manner (mono-space font only) */
void print_structured(tree* t) {
    if (t == NULL) return;

    int height = depth(*t);  // Height of tree
    int max_digits = log10(tree_max(t->root)) + 1;
    int line_width = (pow(2, height) - 1);  // Max output line width

    /* Create print queue */
    queue* print_queue = create_queue();

    /* Enqueue root (could be NULL) */
    enqueue(print_queue, t->root);

    /* Print the tree sctructure level by level */
    for (int i = 0; i < height; i += 1) {
        /* Print leading spaces */
        print_times(" ", max_digits * (pow(2, height - 1 - i) - 1));

        /* Print tree */
        for (int j = 0; j < pow(2, i); j += 1) {
            qnode* first = print_queue->first;
            
            /* If qnode->node is NULL, then it's an empty "filler" qnode (for formatting) */
            if (first->node == NULL) {
                /* Enqueue filler qnodes */
                enqueue(print_queue, NULL);
                enqueue(print_queue, NULL);

                /* Print "empty" value */
                print_times(" ", max_digits);
                print_times(" ", max_digits * (int) (pow(2, height - i) - 1));

                /* Dequeue from 'print_queue' */
                _dequeue_null(print_queue);
            } else {
                /* Enqueue children */
                enqueue(print_queue, first->node->left);
                enqueue(print_queue, first->node->right);

                /* Dequeue from 'print_queue' */
                int num = dequeue(print_queue);
                int num_digits = (num == 0) ? 1 : (int) (log10(abs(num)) + 1);

                /* Print zero-filled num and trailing space */
                print_times("0", max_digits - num_digits);
                printf("%i", num);
                print_times(" ", max_digits * (int) (pow(2, height - i) - 1));
            }
        }

        /* Print a trailing new-line */
        printf("\n");
    }
}

/* ~~~~~ */

int main(void) {
    // tree* bst = create_tree();

    // tree_remove(bst, 69);

    // add(bst, 7);
    // add(bst, 3);
    // add(bst, 1);
    // add(bst, 5);
    // add(bst, 0);
    // add(bst, 2);
    // add(bst, 4);
    // add(bst, 6);
    // add(bst, 11);
    // add(bst, 9);
    // add(bst, 8);
    // add(bst, 10);
    // add(bst, 12);
    // add(bst, 13);
    // add(bst, 14);

    // print_structured(bst1);

    // print_in_order(*bst);
    // print_bft(bst);
    // printf("\n");

    // print_structured(bst);
    // printf("\n");

    // printf("Tree min: %i\n", tree_min(bst->root));
    // printf("Tree max: %i\n", tree_max(bst->root));
    // printf("Tree max left: %i\n", tree_max(bst->root->left));
    // printf("\n");

    // tree_remove(bst, 12);
    // print_structured(bst);
    // printf("\n");

    // tree_remove(bst, 13);
    // tree_remove(bst, 13);
    // print_structured(bst);
    // printf("\n");

    // tree_remove(bst, 7);
    // print_structured(bst);
    // printf("\n");

    // free_tree(bst);

    return 0;
}
