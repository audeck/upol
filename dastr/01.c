#include <stdio.h>
#include <stdlib.h>

// insert, remove-min, union

#define max(a, b) ((a > b) ? a : b)
#define heap_left(a) ((2 * a) + 1)
#define heap_right(a) ((2 * a) + 2)
#define heap_parent(a) (ceil(j/2) - 1) // TODO?

typedef struct {
    int* data;
    int size;
} binary_heap;

void min_heapify(binary_heap* heap, int i) {
    int smallest = i;
    int left = heap_left(i);
    int right = heap_right(i);

    if (left < heap->size && heap->data[left] < heap->data[smallest]) {
        smallest = left;
    }

    if (right < heap->size && heap->data[right] < heap->data[smallest]) {
        smallest = right;
    }

    if (smallest != i) {
        int temp = heap->data[i];
        heap->data[i] = heap->data[smallest];
        heap->data[smallest] = temp;

        min_heapify(heap, smallest);
    }
}

void decrease_key(binary_heap* heap, int i, int key) {
    if (heap->data[i] < key) {
        // TODO: Error
        return;
    }

    int index = i;
    heap->data[i] = key;

    while (i > 0 && heap->data[heap_parent(i)] > heap->data[i]) {
        int temp = heap->data[i];
        heap->data[i] = heap->data[heap_parent(i)];
        heap->data[heap_parent(i)] = temp;
        index = heap_parent(index);
    }
}

void insert(binary_heap* heap, int i) {
    
}

int remove_min(binary_heap* heap) {
    if (heap->size < 1) {
        return null;  // TODO
    }

    int min = heap->data[0];
    heap->data[0] = heap->data[heap->size];
    heap->size -= 1;
    min_heapify(heap, 0);

    return min;
}
