#include <stdio.h>
#include <stdlib.h>



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~ */

typedef struct binomial_tree {
    int key;
    int degree;

    binomial_tree* child;
    binomial_tree* sibling;
    binomial_tree* parent;
} binomial_tree;

typedef struct {
    binomial_tree* root;
} binomial_heap;



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ SOURCE CODE ~~~~~~~~~~~~~~~~~~~~~~~~~ */
