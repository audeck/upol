#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define max(a, b) ((a > b) ? a : b)

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
        fprintf(stderr, "[ERROR in create_tree()]: Failed to allocate tree");
        return NULL;
    }

    ptr->root = NULL;

    return ptr;
}


void rotate_right(node* root) {
    node* prev_root = root;
    node* new_root = root->left;

    /* Update parenting */
    new_root->parent = prev_root->parent;
    prev_root->parent = new_root;

    if (prev_root->parent != NULL) {
        if (prev_root->parent->left == prev_root) {
            prev_root->parent->left = new_root;
        } else {
            prev_root->parent->right = new_root;
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

    /* Update parenting */
    new_root->parent = prev_root->parent;
    prev_root->parent = new_root;

    if (prev_root->parent != NULL) {
        if (prev_root->parent->left == prev_root) {
            prev_root->parent->left = new_root;
        } else {
            prev_root->parent->right = new_root;
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
        y->bf += 1 - (2 * (x == y->right));  // += 1 or += -1
        printf("%i\n", y->bf);

        switch (y->bf) {
            case 0:
                return;

            case 2:
                if (y->left->bf >= 0) {
                    rotate_right(y);
                } else {
                    rotate_left(y->left);
                    rotate_right(y);
                }

                if (y == t->root) {
                    t->root = x;
                }

                break;

            /* Should be case 2 mirrored */
            case -2:
                if (y->right->bf <= 0) {
                    rotate_left(y);
                } else {
                    rotate_right(y->right);
                    rotate_left(y);
                }

                if (y == t->root) {
                    t->root = x;
                }
                
                break;
        }

        /* Shift 'x' and 'y' upwards */
        x = y;
        y = y->parent;
    }
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

/* Dequeues a qnode with node == NULL (and doesn't return anything) - used exclusivelly in print_structured() */
void _dequeue_null(queue* q) {
    if (q->first == NULL) {
        fprintf(stderr, "[dequeue_null() (WARNING)] Tried to dequeue from an empty queue\n");
    }

    qnode* popped = q->first;
    q->first = popped->twrd_last;  // Don't have to NULL q->last with right enqueue
    free(popped);
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

/* Prints 'string' to stdout 'times' times */
void print_times(char* string, int times) {
    for (int i = 0; i < times; i += 1) {
        printf("%s", string);
    }
}

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

    free(print_queue);
}



int main(void) {
    tree* AVL = create_tree();
    add_node(AVL, 1);
    add_node(AVL, 2);
    add_node(AVL, 3);
    //add_node(AVL, 4);
    //add_node(AVL, 5);

    print_structured(AVL);

    return 0;
}
