#include <iostream>
#include <random>
#include <cstring>



/* ========================= Pole ========================= */

class Pole {
 protected:
  int* pole = nullptr;
  int rozsah = 0;
  int zaplneni = 0;

 public:
  explicit Pole(int r) : pole(new int[r]), rozsah(r) {}

  ~Pole() {
    delete[] pole;
  }

  virtual int najit(int) const = 0;
  virtual int pridat(int) = 0;
  virtual bool odebrat(int) = 0;

  /**
   * Vypise pole do stdout (cout) ve formatu:
   * "pole[0]__pole[1]__pole[2]__..." kde _ jsou mezery
   */
  void vypsat() {
    for (int i = 0; i < zaplneni; i += 1) {
      std::cout << pole[i] << "  ";

      if ((i + 1) % 10 == 0 || (i + 1) == zaplneni) {
        std::cout << std::endl;
      }
    }
  }
};



/* ========================= UsporadanePole ========================= */

class UsporadanePole : public Pole {
 public:
  explicit UsporadanePole(int rozsah) : Pole(rozsah) {}

  /**
   * Pokusi se najit `cislo` ve svem poli a vratit jeho index.
   * Vrati -1 pokud cislo nebylo najito (take pro prazdne pole).
   * 
   * @param cislo cislo k najiti
   * @return index `cislo` pokud existuje v poli, jinak -1
   */
  int najit(int cislo) const override {
    // Pokud je pole prazdne
    if (zaplneni == 0) {
      return -1;
    }

    int mozny_index = binarni_vyhledani(cislo);
    return (pole[mozny_index] == cislo) ? mozny_index : -1;
  }

  /**
   * Prida `cislo` do sveho pole, pokud tam uz neni, a vrati 1.
   * Pokud hodnota `cislo` uz v poli existuje, vrati 0.
   * Pokud je pole plne, vrati -1.
   * 
   * @param cislo cislo na pridani
   * @return -1, 0, nebo 1
   */
  int pridat(int cislo) override {
    // Pokud je pole plne
    if (zaplneni == rozsah) {
      return -1;
    }

    // Pokud je pole prazdne
    if (zaplneni == 0) {
      _pridat(cislo, 0);
      return 1;
    }

    // Vyhledej cislo v poli
    int mozny_index = binarni_vyhledani(cislo);

    // Pokud byl najit pouze naslednik cisla
    if (pole[mozny_index] != cislo) {
      _pridat(cislo, mozny_index);
      return 1;
    }

    // Cislo najito
    return 0;
  }

  /**
   * Odstrani `cislo` ze sveho pole a posune vsechna nasledujici
   * cisla doleva. Vraci -1 pokud `cislo` v poli neni.
   * 
   * @param cislo cislo k odebrani
   * @return true pokud cislo bylo odebrano, jinak false
   */
  bool odebrat(int cislo) override {
    int index = najit(cislo);

    if (index != -1) {
      memmove(
        pole + index, 
        pole + index + 1, 
        (zaplneni - (index + 1)) * sizeof(int)
      );
      zaplneni -= 1;

      return true;
    }

    return false;
  }

  bool je_usporadane() {
    for (int i = 0; i < zaplneni - 1; i += 1) {
      if (pole[i] > pole[i + 1]) {
        return false;
      }
    }
    return true;
  }

 private:
  /**
   * Vyhleda ve svem poli `cislo` a vrati jeho index, nebo index
   * nejnizsiho vetsiho cisla v poli. Specialne vrati -1 pokud je 
   * pole prazdne.
   * 
   * @param cislo cislo, ktere hleda
   * @return index cisla, popr. index jeho naslednika v poli, jinak -1
   */
  int binarni_vyhledani(int cislo) const {
    // Pokud je pole prazdne
    if (zaplneni == 0) {
      return -1;
    }

    // Bezny binary search
    int left = 0;
    int right = zaplneni - 1;
    int mid;

    while (left <= right) {
      mid = left + (right - left) / 2;

      if (pole[mid] == cislo) {
        return mid;
      } else if (pole[mid] > cislo) {
        right = mid - 1;
      } else {
        left = mid + 1;
      }
    }

    // Vrat naslednika cisla
    return (cislo < pole[mid]) ? mid : mid + 1;
  }

  /**
   * Posune vsechna cisla od `index` po zaplneni ve svem poli,
   * ulozi `cislo` na `index`, a inkrementuje zaplneni.
   * 
   * @param cislo vlozene cislo
   * @param index index nove vlozeneho cisla
   */
  void _pridat(int cislo, int index) {
    // Prenes cisla od indexu do zaplneni o jedno dal
    memmove(
      pole + index + 1, 
      pole + index, 
      (zaplneni - index) * sizeof(int)
    );

    // Uloz `cislo` na `index`
    pole[index] = cislo;

    // Inkrementuj zaplneni
    zaplneni += 1;
  }
};



/* ========================= Testy ========================= */

void test() {
  // Zakladni test
  UsporadanePole up(4);
  up.vypsat();

  up.pridat(3);
  std::cout << "~ Pole po pridani 3 ~" << std::endl;
  up.vypsat();

  up.pridat(1);
  up.pridat(6);
  std::cout << "~ Pole po druhem pridani 1 a 6 ~" << std::endl;
  up.vypsat();

  if (up.pridat(3) != 0) {
    std::cout << "### Chyba pri druhem pridani 3! ###" << std::endl;
  }
  std::cout << "~ Pole po druhem pridani 3 ~" << std::endl;
  up.vypsat();

  up.pridat(9);
  if (up.pridat(0) != -1) {
    std::cout << "### Chyba pri pridavani do plneho pole! ###" << std::endl;
  }

  up.odebrat(3);
  if (up.odebrat(4) != false) {
    std::cout << "Chyba pri odebirani nepritomneho prvku!" << std::endl;
  }
  std::cout << "~ Pole po pridani 9 a odebrani 3 ~" << std::endl;
  up.vypsat();

  up.odebrat(1);
  up.odebrat(6);
  up.odebrat(9);
  std::cout << "~ Pole by melo byt prazdne ~" << std::endl;
  up.vypsat();



  // Test usporadani
  int size = 100;
  int num = 1000;
  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_int_distribution<> dis(0, 999);

  for (int i = 0; i < num; i += 1) {
    UsporadanePole* up = new UsporadanePole(size);

    for (int i = 0; i < size; i += 1) {
     up->pridat(dis(gen));
    }

    bool sorted = up->je_usporadane();
    delete up;

    if (!sorted) {
      std::cout << "Pole " << i << " bylo spatne usporadano!" << std::endl;
      break;
    }
  }
}



/* ========================= Main ========================= */

int main() {
  // test();

  return 0;
}