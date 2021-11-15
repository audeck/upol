/* 
   TRETI BONUSOVY UKOL ZE ZP1

   Reseni odevzdejte do 22.11.2020 13:59 na email: petr.osicka@upol.cz 
   s predmetem Bonusovy ukol 3, .c soubor prilozte k mailu, 
   do tela mailu napiste sve jmeno.
  

   ZADANI UKOLU:

   Naprogramujte nasledujici funkce (kazda je za jeden bod). 

   void justify_left (char src[], int line_len);
   void justify_right (char src[], int line_len);
   
   Parametr src je retezec. Parametr line_len urcuje delku jednoho
   tiskoveho radku jako pocet vytistenych znaku.

   Ukolem funkci je obsah src vytisknout na obrazovku tak, aby jeden radek nepresahoval
   delku line_len, ale soucasne byl co nejdelsi (tj. pridanim dalsiho slova uz bychom 
   delku line_len presahli). Radky nelze lamat v polovine slova, 
   za slovo povazujeme posloupnost znaku, ktera neni
   prerusena bilym znakem: mezera, novy radek, tabulator.

   Funkce muze tisknout mene nebo vice bilych znaku nez je obsazeno
   v retezci src, musi ovsem zachovat stejna slova (tj. nelze 
   nevytisknout alespon jeden bily znak mezi dvema slovy).

   Funkce justify_left provede tisk tak, ze vsechny radky budou
   zacinat v prvnim textovem sloupci, tj. text je zarovnan doleva.

   Funkce justify_right provede tisk tak, ze vsechny radky budou
   koncit v poslednim textovem sloupci, tj. text je zarovnan doprava.

   Priklad zarovnani vlevo a vpravo

   ------------------------------
   Vzorovy text ukazuje priklad
   zarovnani vlevo
   
   ------------------------------
     Vzorovy text ukazuje priklad
                 zarovnani vpravo       

   Muzete predpokladat, ze nejdelsi slovo obsazene v src je kratsi
   nebo stejne dlouhe jako line_len.

   Z knihovny string.h je povolena pouze funkce strlen.

 */
