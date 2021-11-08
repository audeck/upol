/* 

   DRUHY BONUSOVY UKOL ZE ZP1 

   Reseni odevzdejte do 1.11.2020 13:59 na email: petr.osicka@upol.cz 
   s predmetem  Bonusovy ukol 2, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.


   ZADANI UKOLU:
   
   Napiste program, ktery pro zadany text a vzor, oba jsou to textove retezce, 
   nalezne vsechny vyskyty retezce vzor v retezci text. Pro kazdy takovy vyskyt vypise
   na obrazovku kolikatym pismenem v textu dany vyskyt vzoru zacina. Viz priklady 
   a kod nize.

   PRIKLADY:
   
   (1)

   char text[] = "ahoj svete";
   char vzor[] = "svete";

   program vypise:
   6

   (2)
   
   char text[] = "blablabla";
   char vzor[] = "bl";

   program vypise:
   1 4 7

   (3)

   char text[] = "ah";
   char vzor[] = "bkdoalskd";

   program nevypise nic
   

   (4)

   char text[] = "aaaa";
   char vzor[] = "aa";

   program vypise:
   1 2 3

 */


#include <stdio.h>

/* Returns the length of a string (without its trailing 0). */
int stringLength(char *string) {
  int i;
  for (i = 0; string[i]; i++);
  return i;
}

/* Prints out the result (no malloc :c). */
void substringCheck(char *string, char *substring) {
  /* Get (sub)string length */
  int string_length = stringLength(string);
  int substring_length = stringLength(substring);

  /* ??? */
  for (int i = 0; i <= string_length - substring_length; i++) {
    int ok = 1;
    for (int j = 0; j < substring_length; j++) {
      if (string[i + j] != substring[j]) {
        ok = 0;
        break;
      }
    }

    if (ok) {
      printf("%i ", i + 1);
    }
  }

  /* Profit */
  printf("\n");
}

int main () {

  char text[] = "vzorovy text. je to text, ve kterem budu vyhledavat vyskyty nejakeho vzoru";
  char vzor[] = "vzor";

  /* TADY DOPLNIT KOD UKOLU */
  substringCheck(text, vzor);

  /* Test 1: 6 */
  char text1[] = "ahoj svete";
  char vzor1[] = "svete";

  printf("\nTest 1: ");
  substringCheck(text1, vzor1);
  printf("(should be 6)\n");

  /* Test 2: 1 4 7 */
  char text2[] = "blablabla";
  char vzor2[] = "bl";

  printf("\nTest 2: ");
  substringCheck(text2, vzor2);
  printf("(should be 1 4 7)\n");

  /* Test 3: */
  char text3[] = "ah";
  char vzor3[] = "bkdoalskd";

  printf("\nTest 3: ");
  substringCheck(text3, vzor3);
  printf("(should be )\n");

  /* Test 4: 1 2 3 */
  char text4[] = "aaaa";
  char vzor4[] = "aa";

  printf("\nTest 4: ");
  substringCheck(text4, vzor4);
  printf("(should be 1 2 3)\n");
  
  return 0;
}
