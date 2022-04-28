#include <stdio.h>
#include <stdarg.h>

#define SIRKA 4
#define VYSKA 5

/* Should be noted that ##__VA_ARGS__ isn't standard C, but makes the macro more readable */
#define VYTVOR(x, y, ...) (vytvor(x, y, ##__VA_ARGS__, 0));

typedef struct obdelnik {
    double x;
    double y;
    double sirka;
    double vyska;
} obdelnik;

/* Vytvori obdelnik */
obdelnik vytvor(double x, double y, ...) {
    obdelnik rect = {x, y};

    va_list args;
    va_start(args, y);

    rect.sirka = va_arg(args, double);  // Argument type can be int or double

    if (rect.sirka == 0) {
        /* Default sirka, vyska values */
        rect.sirka = SIRKA;
        rect.vyska = VYSKA;

        /* End early */
        va_end(args);
        return rect;
    }

    rect.vyska = va_arg(args, double);

    if (rect.vyska == 0) {
        /* Rectangle is square */
        rect.vyska = rect.sirka;
    }

    va_end(args);
    return rect;
}

/* Vypise obdelnik */
void vypis(obdelnik rect) {
    printf("Obdelnik:\n");
    printf(" - [%.0lf, %.0lf]", rect.x,              rect.y + rect.vyska);
    printf(" [%.0lf, %.0lf]\n", rect.x + rect.sirka, rect.y + rect.vyska);
    printf(" - [%.0lf, %.0lf]", rect.x,              rect.y);
    printf(" [%.0lf, %.0lf]\n", rect.x + rect.sirka, rect.y);
}



int main(void) {
    // obdelnik dflt = VYTVOR(0.0, 0.0);
    // vypis(dflt);
    // obdelnik square = VYTVOR(1.0, 1.0, 3.0);
    // vypis(square);
    // obdelnik rectangle = VYTVOR(-1.0, -1.0, 6.0, 5.0);
    // vypis(rectangle);

    return 0;
}
