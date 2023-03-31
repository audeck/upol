#ifndef BINARY_HEAP_H
#define BINARY_HEAP_H

typedef struct {
    int* data;
    size_t size;
    size_t limit;
} binary_heap;

binary_heap* make_binary_heap();
void destruct(binary_heap* heap);
void print(binary_heap* heap);
void realloc_data(binary_heap* heap, size_t new_limit);
void expand_data_if_needed(binary_heap* heap);
void shrink_data_if_needed(binary_heap* heap);
void swap_keys(binary_heap* heap, size_t index1, size_t index2);
void set_key(binary_heap* heap, size_t index, int key);
void insert(binary_heap* heap, int key);
void min_heapify(binary_heap* heap, int i);
int remove_min(binary_heap* heap);
binary_heap* heap_union(binary_heap* heap1, binary_heap* heap2);
void test_binary_heap();

#endif /* BINARY_HEAP_H */