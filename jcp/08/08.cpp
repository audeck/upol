#include <iostream>
#include <fstream>
#include <algorithm>

// Relativni cesta k binarnimu souboru "Jmena"
#define CESTA_K_SOUBORU "Jmena"
#define POCET_JMEN 134

enum Pohlavi {
  MUZ = 1,
  ZENA
};

struct Jmeno {
  std::string j;
  Pohlavi p;
};

/* Deklarace pole jmen. */
Jmeno J[POCET_JMEN];



/* ------------------------------- Operatory ------------------------------- */

/**
 * Vraci `true` pokud `a.j` < `b.j` (trideni nezaplnenych poli inicializovanych
 * prazdnymi Jmeny); tj. operator < udava vzestupne lexikograficke usporadani
 * objektu `Jmeno` podle `Jmeno.j`.
 * 
 * @param a prvni Jmeno
 * @param b druhe Jmeno
 * 
 * @return true pokud `!a.j.empty()` a navic `a.j < b.j` 
 * @return false jinak
 */
inline bool operator < (const Jmeno& a, const Jmeno& b) {
  return a.j < b.j;
}

/**
 * Uplne(!) porovna dve Jmena.
 * 
 * @param a prvni Jmeno
 * @param b druhe Jmeno
 * 
 * @return true pokud se `a` rovna `b` ve vsech slozkach
 * @return false jinak
 */
inline bool operator == (const Jmeno& a, const Jmeno& b) {
  return a.j == b.j && a.p == b.p;
}

/**
 * Vraci `true` pokud `e` se vyskytuje v `t`, jinak `false`.
 * 
 * @param t pole, ve kterem ma byt prvek najit
 * @param size velikost `t`
 * @param e prvek, ktery ma byt najit
 * 
 * @return true pokud `t` obsahuje `e`
 * @return false jinak
 */
template<typename T>
inline bool contains(T* t, size_t size, T e) {
  return std::find(t, t + size, e) != t + size;
}



/* -------------------------------- Funkce --------------------------------- */

/**
 * Nacte unikatni jmena z binarniho souboru daneho relativni cestou `file_path`
 * do pole `jmena` (velikosti `size`) a pole `jmena` seradi lexikograficky.
 * 
 * @param jmena pole jmen
 * @param size velikost `jmena`
 * @param file_path cesta k binarnimu souboru, ktery ma byt nacten
 */
void load(Jmeno* jmena, size_t size, std::string file_path) {
  // Zkus nacist soubor
  std::ifstream input_file(file_path, std::ios::binary);
  if (!input_file.is_open()) {
    std::cerr << "[ERROR in load_names(Jmeno*, size_t, std::string)]: "
      << "Unable to open file " << file_path << std::endl;
    return;
  }

  // Vycisti `jmena`
  for (int i = 0; i < size; i += 1) {
    Jmeno init;
    jmena[i] = init;
  }

  // Nacti data ze souboru do `jmena`
  size_t index = 0;
  while (index < size && input_file.peek() >= 0) {
    Jmeno jmeno;

    char header = input_file.get();
    
    jmeno.p = (header >= 50) ? ZENA : MUZ;
    header %= 50;  // Sniz header pro zeny na realny pocet znaku

    jmeno.j = std::string();
    // Nacti `header` znaku do `jmeno.j`
    for (int i = 0; i < header; i += 1) jmeno.j += input_file.get();

    if (!contains(jmena, index, jmeno)) {
      jmena[index] = jmeno;
      index += 1;
    }
  }

  // Serad jmena
  std::sort(jmena, jmena + index);
  input_file.close();
}

/**
 * Vytiskne pole `jmena` do `stdout` nasledovne:
 *  - vytiskne jmena zen (10 jmen / radek)
 *  - vytiskne prazdny radek
 *  - vytiskne jmena muzu (10 jmen / radek)
 * 
 * @param jmena reference na pole jmen
 */
void print(Jmeno* jmena, size_t size) {
  int names_written = 0;

  // Vytiskni jmena zen
  for (int i = 0; i < size; i += 1) {
    if (jmena[i].p == ZENA) {
      std::cout << jmena[i].j << " ";
      names_written += 1;
      if (names_written % 10 == 0) std::cout << std::endl;
    }
  }

  std::cout << std::endl << std::endl;
  names_written = 0;

  // Vytiskni jmena muzu
  for (int i = 0; i < size; i += 1) {
    if (jmena[i].p == MUZ) {
      std::cout << jmena[i].j << " ";
      names_written += 1;
      if (names_written % 10 == 0) std::cout << std::endl;
    }
  }

  std::cout << std::endl;
}



/* --------------------------------- Main ---------------------------------- */

int main() {
  load(J, POCET_JMEN, CESTA_K_SOUBORU);
  print(J, POCET_JMEN);

  return 0;
}
