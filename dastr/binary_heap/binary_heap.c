#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <limits.h>



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ DEFINITIONS ~~~~~~~~~~~~~~~~~~~~~~~~~ */

// Binary heap left child index
#define left(a) ((2 * a) + 1)
// Binary heap right child index
#define right(a) ((2 * a) + 2)
// Binary heap parent index
#define parent(a) ((a - 1) / 2)

// Initial array size
#define INIT_SIZE 4
// Array growth factor
#define GROWTH_FACTOR 2

// DEBUG
#define DEBUG 0

// Binary heap struct
typedef struct {
    int* data;
    size_t size;
    size_t limit;
} binary_heap;



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ SOURCE CODE ~~~~~~~~~~~~~~~~~~~~~~~~~ */

/**
 * `binary_heap` type "object" constructor.
 * Allocates a binary_heap struct on the heap and returns it's pointer.
 * 
 * @param limit the "limit size" of the heap's data array
 * @return binary_heap* pointer to the created heap
 */
binary_heap* make_binary_heap() {
    binary_heap* heap = (binary_heap*) malloc(sizeof(binary_heap));
    if (heap == NULL) {
        fprintf(stderr, "[ERROR in make_binary_heap()]: Memory allocation failed.");
        exit(1);
    }

    heap->data = (int*) malloc(INIT_SIZE * sizeof(int));
    if (heap->data == NULL) {
        fprintf(stderr, "[ERROR in make_binary_heap()]: Memory allocation failed.");
        exit(1);
    }

    heap->size = 0;
    heap->limit = INIT_SIZE;
    
    return heap;
}

/**
 * Frees `heap` and it's data.
 * 
 * @param heap heap to be freed/destructed
 */
void destruct(binary_heap* heap) {
    free(heap->data);
    free(heap);
}

/**
 * Prints out a binary_heap to stdout in the format of:
 *  [ e1, e2, ... ], where e1, e2, ... are the heap's data elements.
 * 
 * @param heap heap to be printed
 */
void print(binary_heap* heap) {
    printf("[");
    for (int i = 0; i < heap->size; i += 1) {
        if (i != 0) putchar(',');
        printf(" %i", heap->data[i]);
    }
    printf(" ]\n");
}

/**
 * Reallocates `heap`'s data array to be of size `new_limit`
 * and copies current contents over. Behavior if the current
 * contents don't fit into the new array is undefined.
 * 
 * @param heap heap
 * @param new_limit new data array size
 */
void realloc_data(binary_heap* heap, size_t new_limit) {
    if (DEBUG) {
        printf("Reallocating to %li\n", new_limit);
    }

    // malloc new data location
    int* new_data = (int*) malloc(new_limit * sizeof(int));
    if (new_data == NULL) {
        fprintf(stderr, "[ERROR in realloc_data(binary_heap*, size_t)]: Memory allocation failed.");
        exit(1);
    }

    // Copy data to new destination
    memcpy(new_data, heap->data, heap->size * sizeof(int));

    // Free old data
    free(heap->data);

    // Update heap
    heap->data = new_data;
    heap->limit = new_limit;
}

/**
 * Expands `heap`'s data array if `heap->size + 1` == `heap->limit`.
 * Expands according to the `GROWTH_FACTOR` constant defined in
 * the source file.
 * 
 * @note Should be called before incrementing `heap->size`.
 * @param heap heap
 */
void expand_data_if_needed(binary_heap* heap) {
    // Should be rewritten if inserting more than one element
    // at once into a heap is allowed.
    if (heap->size + 1 > heap->limit) {
        realloc_data(heap, heap->limit * GROWTH_FACTOR);
    }
}

/**
 * Shrinks `heap`'s data array if needed; i.e. if the current
 * amount of elements is equal to `heap->limit / GROWTH_FACTOR`
 * as defined in the source file.
 * 
 * @note Should be called after decrementing `heap->size`.
 * @param heap heap
 */
void shrink_data_if_needed(binary_heap* heap) {
    if (heap->size == heap->limit / GROWTH_FACTOR) {
        realloc_data(heap, heap->limit / GROWTH_FACTOR);
    }
}

/**
 * Swaps the values of two keys (at indexes `index1` & `index2`) in `heap`.
 * 
 * @param heap heap
 * @param index1 index of first key
 * @param index2 index of second key
 */
void swap_keys(binary_heap* heap, size_t index1, size_t index2) {
    int tmp = heap->data[index1];
    heap->data[index1] = heap->data[index2];
    heap->data[index2] = tmp;
}

/**
 * Sets the value of element at `index` in `heap` to the value of `key`.
 * 
 * @param heap heap
 * @param index index of element to be changed
 * @param key new key value
 */
void set_key(binary_heap* heap, size_t index, int key) {
    if (key > heap->data[index]) {
        return;
    }

    heap->data[index] = key;

    while (index > 0 && heap->data[parent(index)] > heap->data[index]) {
        swap_keys(heap, parent(index), index);
        index = parent(index);
    }
}

/**
 * Inserts a key with value `key` into `heap`.
 * 
 * @param heap heap
 * @param key key value to be inserted
 */
void insert(binary_heap* heap, int key) {
    expand_data_if_needed(heap);
    heap->size += 1;
    heap->data[heap->size - 1] = INT_MAX;
    set_key(heap, heap->size - 1, key);
}

/**
 * Ensures that element at index `i` doesn't break `heap`'s
 * "min-heap" constaint (doesn't have to actually change `heap`).
 * 
 * @param heap heap
 * @param i index of element
 */
void min_heapify(binary_heap* heap, size_t i) {
    size_t smallest = i;
    size_t left = left(i);
    size_t right = right(i);

    if (left < heap->size && heap->data[left] < heap->data[smallest]) {
        smallest = left;
    }

    if (right < heap->size && heap->data[right] < heap->data[smallest]) {
        smallest = right;
    }

    if (smallest != i) {
        swap_keys(heap, i, smallest);
        min_heapify(heap, smallest);
    }
}

/**
 * Removes the minimum element in `heap` and returns it. Also ensures
 * that the resulting `heap` without the element follows the "min-heap"
 * constraint.
 * 
 * @param heap heap
 * @return int minimum element in heap
 */
int remove_min(binary_heap* heap) {
    if (heap->size == 0) {
        fprintf(stderr, "[ERROR in remove_min(binary_heap*)]: Tried to remove min from an empty heap.");
        exit(1);
    }

    // Get min
    int min = heap->data[0];

    // Swap last element into heap's root
    heap->data[0] = heap->data[heap->size - 1];
    heap->size -= 1;
    shrink_data_if_needed(heap);

    // min_heapify new root
    min_heapify(heap, 0);

    return min;
}

/**
 * Allocates a new binary_heap on the heap, sets it's content to
 * equal the union of `heap1` and `heap2`, and returns it's pointer.
 * 
 * @param heap1 first heap
 * @param heap2 second heap
 * @return binary_heap* union of `heap1` and `heap2`
 */
binary_heap* heap_union(binary_heap* heap1, binary_heap* heap2) {
    binary_heap* result = make_binary_heap();

    // Unoptimized implementation
    for (int i = 0; i < heap1->size; i += 1) {
        insert(result, heap1->data[i]);
    }

    for (int i = 0; i < heap2->size; i += 1) {
        insert(result, heap2->data[i]);
    }

    return result;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~ */

void test_binary_heap() {
    printf("~~~ Binary heap test ~~~\n");
    binary_heap* bh1 = make_binary_heap();
    insert(bh1, 1);
    insert(bh1, 2);
    insert(bh1, 4);
    insert(bh1, 3);
    insert(bh1, 0);
    insert(bh1, 2);
    insert(bh1, 1);
    print(bh1);

    printf("Min: %i; Heap: ", remove_min(bh1));
    print(bh1);

    printf("Min: %i; Heap: ", remove_min(bh1));
    print(bh1);

    printf("Min: %i; Heap: ", remove_min(bh1));
    print(bh1);

    binary_heap* bh2 = make_binary_heap();
    insert(bh2, 6);
    insert(bh2, 4);
    insert(bh2, 0);
    insert(bh2, 9);
    insert(bh2, 2);
    insert(bh2, 14);
    insert(bh2, 26);
    insert(bh2, 99);

    printf("bh1: ");
    print(bh1);

    printf("bh2: ");
    print(bh2);

    binary_heap* bh = heap_union(bh1, bh2);
    printf("Union of bh1 and bh2: ");
    print(bh);

    destruct(bh);
    destruct(bh1);
    destruct(bh2);
}
