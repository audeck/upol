/* Zadani (abbr.):

/pro n = 3/
  *
 * *
* * *
/pro n = 4/
   *
  * *
 * * *
* * * *

*/

#include <stdio.h>

/* Prints a triangle to stdout (as seen in 'zadani'). */
void triangle(int n) {
    /* Line by line */
    for (int i = 1; i <= n; i++) {
        int current_width = i * 2 - 1;
        int padding = n - i;

        /* Front padding */
        for (int j = 0; j < padding; j++) {
            printf(" ");
        }

        /* Triangle body */
        for (int j = 0; j < current_width; j++) {
            if (j % 2 == 0) {
                printf("*");
            } else {
                printf(" ");
            }
        }

        /* Rear padding (optional formatting) */
        for (int j = 0; j < padding; j++) {
            printf(" ");
        }

        printf("\n");
    }
}

int main(void) {
    printf("Hello, world!\n");

    /* User input not included. */

    triangle(-1);
    triangle(3);
    triangle(7);
}
