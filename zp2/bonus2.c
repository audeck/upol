#include <stdio.h>
#include <stdarg.h>

#define SIRKA 4
#define VYSKA 5

typedef struct obdelnik {
    double x;
    double y;
    double sirka;
    double vyska;
} obdelnik;

obdelnik vytvor(double x, double y, ...) {
    obdelnik o = {x, y};

    va_list args;
    va_start(args, y);

    if ((o.sirka = va_arg(args, double)) == 0) {
        printf("Sirka & vyska == 0!\n");
        o.sirka = SIRKA;
        o.vyska = VYSKA;
    }
    else if ((o.vyska = va_arg(args, double)) == 0) {
        printf("Vyska == 0!\n");
        printf("%lf\n", o.sirka);
        o.vyska = o.sirka;
    }

    va_end(args);

    return o;
}

void vypis(obdelnik rect) {
    printf("Obdelnik:\n");
    printf(" - [%.0lf, %.0lf] [%.0lf, %.0lf]\n", rect.x, rect.y + rect.vyska, rect.x + rect.sirka, rect.y + rect.vyska);
    printf(" - [%.0lf, %.0lf] [%.0lf, %.0lf]\n", rect.x, rect.y, rect.x + rect.sirka, rect.y);
}



int main(void) {
    obdelnik def = vytvor(0, 0, 0);
    vypis(def);
    obdelnik square  = vytvor(1, 1, 3, 0);
    vypis(square);
    obdelnik rectangle = vytvor(-1, -1, 6, 5, 0);
    vypis(rectangle);

    return 0;
}
