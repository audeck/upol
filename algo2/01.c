#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#define ENTRIES 5

struct dir {
    char* name;
    char* surname;
    int age;
    int debt;
};

struct dir debtors[ENTRIES] = {
    {"Dwight", "House",   27, 500},
    {"Hilda",  "Climb",   23, 12345},
    {"Jack",   "Cass",    33, 100000},
    {"Anna",   "Liszt",   25, 543},
    {"Mike",   "Rophone", 24, 1500}
};

/* Prints out every debtor's (of 'name' and 'surname') debt */
void print_debt(struct dir* debtors, char* name, char* surname) {
    for (int i = 0; i < ENTRIES; i++) {
        if (debtors[i].name == name && debtors[i].surname == surname) {
            printf(" - %s %s's debt is %i.\n", name, surname, debtors[i].debt);
        }
    }
}

/* Returns the debtor struct with the highest debt amount */
struct dir max(struct dir* debtors) {
    int debtor_index = 0;

    for (int i = 1; i < ENTRIES; i++) {
        if (debtors[i].debt > debtors[debtor_index].debt) {
            debtor_index = i;
        }
    }

    return debtors[debtor_index];
}

/* Increases every debtor's (of 'name' and 'surname') debt by 'amount' */
void increase_debt(struct dir* debtors, char* name, char* surname, int amount) {
    for (int i = 0; i < ENTRIES; i++) {
        if (debtors[i].name == name && debtors[i].surname == surname) {
            debtors[i].debt += amount;
        }
    }
}

/* Decreases every debtor's (of 'name' and 'surname') debt by 'amount' */
void decrease_debt(struct dir* debtors, char* name, char* surname, int amount) {
    for (int i = 0; i < ENTRIES; i++) {
        if (debtors[i].name == name && debtors[i].surname == surname) {
            debtors[i].debt -= amount;
        }
    }
}

/* 
    Usual randomized quick partition.
    Mutates the original array - could copy 'adresar' first
*/
struct dir* _partition(struct dir* debtors, int l, int r, int n) {
    /* Get random index */
    srand((unsigned int) time(NULL));
    int rand_index = rand() % (r - l + 1) + l;

    /* Swap "random" pivot for last element */
    struct dir tmp = debtors[r];
    debtors[r] = debtors[rand_index];
    debtors[rand_index] = tmp;

    /* Quick partition */
    int pivot_index = l;
    for (int i = l; i < r; i++) {
        if (debtors[i].age < debtors[r].age) {
            tmp = debtors[i];
            debtors[i] = debtors[pivot_index];
            debtors[pivot_index] = tmp;
            pivot_index += 1;
        }
    }

    /* Swap in pivot */
    tmp = debtors[r];
    debtors[r] = debtors[pivot_index];
    debtors[pivot_index] = tmp;

    /* Return either the recursive call of '_partition' or the n-th debtor struct address */
    if (pivot_index > n - 1) return _partition(debtors, l, pivot_index - 1, n);
    if (pivot_index < n - 1) return _partition(debtors, pivot_index + 1, r, n);
    return &debtors[pivot_index];
}

/* Returns the n-th youngest debtor */
struct dir nth_youngest(struct dir* debtors, int n) {
    if (n > ENTRIES) {
        fprintf(stderr, "[nth_youngest (ERROR)]: n is out of bounds (n > ENTRIES)");
    } else {
        return *_partition(debtors, 0, ENTRIES - 1, n);
    }
}

/* Sorts the struct array by age using insertion sort (lazy, I know) */
void sort(struct dir* debtors) {
    for (int i = 1; i < ENTRIES; i++) {
        struct dir tmp = debtors[i];
        int j = i - 1;

        while (tmp.age < debtors[j].age && j >= 0) {
            debtors[j + 1] = debtors[j];
            j--;
        }

        debtors[j + 1] = tmp;
    }
}



int main(void) {
    // print_debt(debtors, "Dwight", "House");
    // print_debt(debtors, "Anna", "Liszt");

    // struct dir max_debtor = max(debtors);
    // printf(" - %s %s has the highest debt! (%i)\n", max_debtor.name, max_debtor.surname, max_debtor.debt);
    
    // increase_debt(debtors, max_debtor.name, max_debtor.surname, 1000);
    // max_debtor = max(debtors);
    // printf(" - %s %s's debt has increased! (%i)\n", max_debtor.name, max_debtor.surname, max_debtor.debt);

    // struct dir youngest = nth_youngest(debtors, 2);
    // printf(" - The 2nd youngest debtor is %s %s! (age %i)\n", youngest.name, youngest.surname, youngest.age);

    // sort(debtors);
    // printf(" - SORTED:\n");
    // for (int i = 0; i < ENTRIES; i++) {
    //     printf("    - %s %s aged %i.\n", debtors[i].name, debtors[i].surname, debtors[i].age);
    // }

    // return 0;
}