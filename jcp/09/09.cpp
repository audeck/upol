#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cctype>
#include <iomanip>

// Cesta ke vstupnimu souboru
#define PATH_TO_FILE "Osoby"



/* --------------------------------- Osoba --------------------------------- */

struct Osoba {
  std::string jmeno, prijmeni;
  
  Osoba() : jmeno(), prijmeni() { }
  Osoba(const char* j, const char* p) : jmeno(j), prijmeni(p) { }
  ~Osoba() { }

  inline bool operator < (const Osoba& osoba) const {
    return this->prijmeni < osoba.prijmeni
      || ((this->prijmeni == osoba.prijmeni) && (this->jmeno < osoba.jmeno));
  }

  inline bool operator () (const char* j, const char* p) const {
    return this->jmeno == j && this->prijmeni == p;
  }
};



/* -------------------------------- Funkce --------------------------------- */

/**
 * Zahodi vsechny uvodni "whitespace" znaky ze streamu `input_file` a vrati
 * `true` pokud neni na konci souboru (i.e. stream obsahuje znak, ktery neni 
 * whitespace), jinak `false`.
 * 
 * @param input_file stream vstupniho souboru
 * 
 * @return true pokud `input_file` obsahuje aspon jeden non-whitespace znak,
 * @return false pokud je vysledny stream na konci souboru
 */
bool contains_non_whitespace(std::ifstream& input_file) {
  // Podivej se na aktualni znak
  char current_char = input_file.peek();

  while (current_char != std::ifstream::traits_type::eof()) {
    // Vrat `true` pokud neni whitespace
    if (!std::isspace(current_char)) { return true; }

    // Jinak zahod aktualni znak a podivej se na dalsi
    input_file.get();
    current_char = input_file.peek();
  }

  // Vrat `false` kdyz na konci souboru
  return false;
}



/**
 * Vraci `true` pokud `osoby` obsahuje osobu se jmenem identickym s `j` a
 * prijmenim identickym s `p`.
 * 
 * @param osoby vektor osob
 * @param j C retezec udavajici jmeno
 * @param p C retezec udavajici prijmeni
 * 
 * @return true pokud `osoby` obsahuje osobu shodnou s Osoba(`j`, `p`),
 * @return false jinak
 */
bool contains(const std::vector<Osoba> osoby, const char* j, const char* p) {
  for (Osoba osoba : osoby) {
    if (osoba(j, p)) { return true; }
  }

  return false;
}



/**
 * Nacte objekty typu `Osoba` ze souboru daneho `file_path` do std::vektoru
 * `osoby`. Nepridava duplictni osoby.
 * 
 * @param osoby vektor osob
 * @param file_path cesta k souboru dana retezcem
 */
void load_from_file(std::vector<Osoba>& osoby, const std::string file_path) {
  // Soubor
  std::ifstream input_file(file_path);
  if (!input_file.is_open()) {
    std::cerr << "[ERROR in load_from_file(std::vector<Osoba>, std::string)]: "
      << "Unable to open file " << file_path << std::endl;
    return;
  }

  // Vektor
  while (contains_non_whitespace(input_file)) {
    std::string jmeno;
    std::string prijmeni;

    input_file >> jmeno;
    input_file >> prijmeni;

    if (!contains(osoby, jmeno.c_str(), prijmeni.c_str())) {
      osoby.emplace_back(Osoba(jmeno.c_str(), prijmeni.c_str()));
    }
  }
}



/**
 * Vytiskne `osoby` na `stream` ve ctyrech sloupcich nasledujiciho schema:
 * 
 *  | osoba.jmeno |_| osoba.prijmeni |__...
 *        ...              ...
 * 
 * kde '_' jsou mezery. Sirka sloupcu osoba.jmeno je nejvetsi delka jmena
 * v `osoby`, sirka osoba.prijmeni nejvetsi delka prijmeni v `osoby`.
 * 
 * @param stream vystupni stream (ktery podporuje manipulatory streamu)
 * @param osoby vektor osob
 */
void print(std::ostream& stream, const std::vector<Osoba> osoby) {
  // Nejvetsi delka jmena
  size_t jmeno_max_sirka = std::max_element(
    osoby.begin(),
    osoby.end(),
    [](const Osoba& prvni, const Osoba& druha) {
      return prvni.jmeno.length() < druha.jmeno.length();
    }
  )->jmeno.length();

  // Nejvetsi delka prijmeni
  size_t prijmeni_max_sirka = std::max_element(
    osoby.begin(),
    osoby.end(),
    [](const Osoba& prvni, const Osoba& druha) {
      return prvni.prijmeni.length() < druha.prijmeni.length();
    }
  )->prijmeni.length();

  // Vypis osob
  unsigned char pocet_osob = 0;
  stream << std::right;  // Zarovnani vpravo

  for (Osoba osoba : osoby) {
    stream << std::setw(jmeno_max_sirka) << osoba.jmeno << " ";
    stream << std::setw(prijmeni_max_sirka) << osoba.prijmeni << "  ";

    if ((pocet_osob += 1) == 4) {
      stream << std::endl;
      pocet_osob = 0;
    }
  }

  // Posledni 'std::endl' pokud je posledni radek neuplny
  if (pocet_osob != 0) { stream << std::endl; }
}



/* --------------------------------- Main ---------------------------------- */

int main() {
  std::vector<Osoba> osoby;
  load_from_file(osoby, PATH_TO_FILE);
  std::sort(osoby.begin(), osoby.end());
  print(std::cout, osoby);

  return 0;
}
