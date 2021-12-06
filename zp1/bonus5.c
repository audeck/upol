/* 
   PATY BONUSOVY UKOL ZE ZP1

   Reseni odevzdejte do 3.1. 2021 23:59 na email: petr.osicka@upol.cz 
   s předmětem Bonusovy ukol 5, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.

   ZADANI UKOLU.

   Binarni relace na mnozine a jeji vlastnosti jsou probirany v 
   predmetu diskretni struktury. Informace lze nalezt i na nasledujich
   strankach na wikipedii. 

   https://cs.wikipedia.org/wiki/Bin%C3%A1rn%C3%AD_relace
   https://cs.wikipedia.org/wiki/Tranzitivn%C3%AD_uz%C3%A1v%C4%9Br

   Uvazme mnozinu X = {0,1,2,3,4, ... M-1}. (M je tedy velikost mnoziny X).

   Binarni relaci R na X muzeme reprezentovat jako matici o 
   M radcich a M sloupcich. Pokud je na radku i ve sloupci j hodnota 1,
   znamena to, ze (i,j) patri do R. Pokud je tam 0, znamena to, ze 
   (i,j) nepatri do R. 

   V programu matice reprezentujte pomoci pole. (o tom jak reprezentovat dvourozmernou
   matici pomoci jednorozmerneho pole jsme mluvili na zacatku seminare).
   Velikost M zavedte pomoci #define.

   Naprogramujte nasledujici funkce.
   
   int reflexive_p(int r[]);
   int symetric_p(int r[]);
   int antisymetric(int r[]);
   int transitive_p(int r[]);

   ktere pro r otestuji, jestli je r reflexivni, symetricka, antisymetricka 
   ci tranzitivni (viz nazev funkce) a vysledek vrati jako navratovou hodnotu.

   Dale naprogramujte funkci

   void transitive_closure(int r[])

   která doplní r tak, aby byla nejmensi nadrelaci puvodniho r, ktera je 
   tranzitivni.

*/

#include <stdio.h>

#define M 8

/* --- Function definitions --- */

/* Returns the 1d array index1D of a 2d array index1D pair */
int index1D(int i, int j) {
    return (i * M) + j;
}

/* Returns 1 if 'relation' is reflexive, returns 0 if it isn't */
int reflexivePred(int relation[]) {
    for (int i = 0; i < M; i++) {
        if (!relation[index1D(i, i)]) {
            return 0;
        }
    }

    return 1;
}

/* Returns 1 if 'relation' is symetric, returns 0 if it isn't */
int symetricPred(int relation[]) {
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < M; j++) {
            if (relation[index1D(i, j)] && !relation[index1D(j, i)]) {
                return 0;
            }
        }
    }

    return 1;
}

/* Returns 1 if 'relation' is antisymetric, returns 0 if it isn't */
int antisymetricPred(int relation[]) {
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < M; j++) {
            if (j != i && relation[index1D(i, j)] && relation[index1D(j, i)]) {
                return 0;
            }
        }
    }

    return 1;
}

/* Returns ((a, b) && (b, c1) => (a, c1)) && ((a, b) && (b, c2) => (a, c2)) && ... */
int checkTransitives(int relation[], int a, int b) {
    for (int c = 0; c < M; c++) {
        if (relation[index1D(b, c)] && !relation[index1D(a, c)]) {
            return 0;
        }
    }

    return 1;
}

/* Returns 1 if 'relation' is transitive, returns 0 if it isn't */
int transitivePred(int relation[]) {
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < M; j++) {
            if (relation[index1D(i, j)] && !checkTransitives(relation, i, j)) {
                return 0;
            }
        }
    }

    return 1;
}

/* Sets the value of (a, c) (i.e. relation[a][c]) to 1 if (a, b) && (b, c) */
void addTransitives(int relation[], int a, int b) {
    for (int c = 0; c < M; c++) {
        if (relation[index1D(b, c)]) {
            relation[index1D(a, c)] = 1;
        }
    }
}

/* Modifies 'relation' to be it's transitive closure (i.e. the smallest superrelation that is transitive) */
void transitiveClosure(int relation[]) {
    while (!transitivePred(relation)) {
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < M; j++) {
                if (relation[index1D(i, j)]) {
                    addTransitives(relation, i, j);
                }
            }
        }
    }
}

/* --- Pipes --- */

int reflexive_p(int r[]) {
    return reflexivePred(r);
};

int symetric_p(int r[]) {
    return symetricPred(r);
};

int antisymetric(int r[]) {
    return antisymetricPred(r);
};

/* In case the lack of "_p" was a typo :^) */
int antisymetric_p(int r[]) {
    return antisymetricPred(r);
};

int transitive_p(int r[]) {
    return transitivePred(r);
};

void transitive_closure(int r[]) {
    transitiveClosure(r);
}

/* --- Main --- */

int main(void) {
    // int test1[M*M] = {
    //     1, 0, 0, 0, 0, 0, 0, 0,
    //     0, 1, 0, 0, 0, 0, 0, 0,
    //     0, 0, 1, 0, 0, 0, 0, 0,
    //     0, 0, 0, 1, 0, 0, 0, 0,
    //     0, 0, 0, 0, 1, 0, 0, 0,
    //     0, 0, 0, 0, 0, 1, 0, 0,
    //     0, 0, 0, 0, 0, 0, 1, 0,
    //     0, 0, 0, 0, 0, 0, 0, 1
    // };

    // printf("~~~~~~ Test 1 ~~~~~~\n");
    // printf("Reflexive:     %s\n", reflexivePred(test1) ? " true" : "false");
    // printf("Symetric:      %s\n", symetricPred(test1) ? " true" : "false");
    // printf("Antisymetric:  %s\n", antisymetricPred(test1) ? " true" : "false");
    // printf("Transitive:    %s\n", transitivePred(test1) ? " true" : "false");

    // printf("\n");

    // int test2[M*M] = {
    //     1, 0, 0, 0, 0, 0, 0, 0,
    //     0, 1, 0, 0, 0, 1, 0, 0,
    //     0, 0, 1, 0, 0, 0, 0, 0,
    //     0, 0, 0, 1, 0, 0, 0, 0,
    //     0, 0, 0, 0, 1, 0, 0, 0,
    //     0, 0, 0, 0, 0, 1, 0, 0,
    //     0, 0, 0, 0, 0, 0, 1, 0,
    //     0, 0, 0, 0, 0, 0, 0, 1
    // };

    // printf("~~~~~~ Test 2 ~~~~~~\n");
    // printf("Reflexive:     %s\n", reflexivePred(test2) ? " true" : "false");
    // printf("Symetric:      %s\n", symetricPred(test2) ? " true" : "false");
    // printf("Antisymetric:  %s\n", antisymetricPred(test2) ? " true" : "false");
    // printf("Transitive:    %s\n", transitivePred(test2) ? " true" : "false");

    // printf("\n");

    // int test3[M*M] = {
    //     1, 0, 0, 0, 0, 0, 0, 0,
    //     0, 1, 0, 0, 0, 1, 0, 0,
    //     0, 0, 1, 0, 0, 0, 0, 0,
    //     0, 0, 0, 1, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 1, 0, 0, 0, 1, 0, 0,
    //     0, 0, 0, 0, 0, 0, 1, 0,
    //     0, 0, 0, 0, 0, 0, 0, 1
    // };

    // printf("~~~~~~ Test 3 ~~~~~~\n");
    // printf("Reflexive:     %s\n", reflexivePred(test3) ? " true" : "false");
    // printf("Symetric:      %s\n", symetricPred(test3) ? " true" : "false");
    // printf("Antisymetric:  %s\n", antisymetricPred(test3) ? " true" : "false");
    // printf("Transitive:    %s\n", transitivePred(test3) ? " true" : "false");

    // printf("\n");

    // int test4[M*M] = {
    //     1, 0, 0, 1, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 1, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0,
    //     0, 0, 0, 0, 0, 0, 0, 0
    // };

    // printf("~~~~~~ Test 4 ~~~~~~\n");
    // printf("Reflexive:     %s\n", reflexivePred(test4) ? " true" : "false");
    // printf("Symetric:      %s\n", symetricPred(test4) ? " true" : "false");
    // printf("Antisymetric:  %s\n", antisymetricPred(test4) ? " true" : "false");
    // printf("Transitive:    %s\n", transitivePred(test4) ? " true" : "false");

    // printf("\n");

    // transitiveClosure(test4);
    // printf("~~~ TC (test 4) ~~~");
    // for (int i = 0; i < M*M; i++) {
    //     if (i % 8 == 0) {
    //         printf("\n  ");
    //     }
    //     printf("%i ", test4[i]);
    // }

    // printf("\n");

    return 0;
}
