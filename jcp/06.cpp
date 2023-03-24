#include <iostream>
#include <iterator>
#include <cstring>



/* ------------------------- TRIDY ------------------------- */

/**
 * Trida reprezentujici mnozinu cisel (typu int32). Trida pretezuje nektere
 * ze zakladnich operatoru, cimz implementuje zakladni funkcionalitu.
 */
class Mnozina {
  protected:
    int32_t* pole;
    size_t rozsah;
    size_t zaplneni;

  public:
    explicit Mnozina(size_t r) : pole(new int32_t[r]), rozsah(r), zaplneni(0) {}
    ~Mnozina() { delete[] pole; }

    /**
     * Prida `element` do mnoziny, pokud tam uz neni.
     * 
     * @param element prvek k pridani
     * @return Mnozina& reference na tento objekt
     */
    Mnozina& operator << (int32_t element) {
        if (zaplneni < rozsah && !obsahuje(element)) {
            pole[zaplneni] = element;
            zaplneni += 1;
        }

        return *this;
    }

    /**
     * Prida cisla z `mnozina` do teto mnoziny.
     * Pokud se pole teto mnoziny zaplni, prestane pridavat
     * (tj. prida jen podmnozinu `mnozina`).
     * 
     * @param mnozina mnozina k pridani
     * @return Mnozina& reference na tento objekt
     */
    Mnozina& operator += (Mnozina& mnozina) {
        for (int32_t prvek : mnozina) {
            if (!obsahuje(prvek)) {
                *this << prvek;
            }
        }

        return *this;
    }

    /**
     * Odebere `element` z mnoziny, pokud se nachazi
     * v poli mnoziny.
     * 
     * @param element element k odebrani
     * @return Mnozina& reference na tento objekt
     */
    Mnozina& operator -= (int32_t element) {
        if (obsahuje(element)) {
            size_t index = index_of(element);
            memmove(
                pole + index,
                pole + index + 1,
                (zaplneni - (index + 1)) * sizeof(int32_t)
            );
            zaplneni -= 1;
        }

        return *this;
    }

    /**
     * Provede mnozinovy rozdil & assign.
     * 
     * @param mnozina "mensitel"
     * @return Mnozina& reference na tento objekt
     */
    Mnozina& operator -= (Mnozina& mnozina) {
        for (int32_t prvek : mnozina) {
            *this -= prvek;
        }

        return *this;
    }

    /**
     * Vraci pocet prvku v mnozine.
     * 
     * @return size_t pocet prvku v mnozine
     */
    size_t operator + () const {
        return zaplneni;
    }

    /**
     * Vypise obsah pole mnoziny do `std::cout`.
     * 
     * @param sirka_radku maximalni pocet prvku na radku
     */
    void operator () (unsigned sirka_radku) const {
        // Zacatek
        std::cout << "Mnozina:" << std::endl;

        // Obsah pole
        for (size_t i = 0; i < zaplneni; i += 1) {
            if (i % sirka_radku == 0 && i != 0) {
                std::cout << std::endl;
            }
            std::cout << "  " << pole[i];
        }

        // Konec
        std::cout << std::endl;
    }

  private:
    /** Vraci true pokud mnozina obsahuje `element`, jinak vraci false. */
    bool obsahuje(int32_t element) const {
        for (size_t i = 0; i < zaplneni; i += 1) {
            if (element == pole[i]) return true;
        }

        return false;
    }

    /**
     * Vraci index `element` v poli mnoziny. Funkce musi byt volana na prvek,
     * ktery v poli je (tj. musi byt zaruceno `obsahuje(element)`)!
     * 
     * @param element element, jehoz index ma byt najit
     * @return size_t index elementu v poli
     * @throws std::logic_error pokud element v poli neexistuje
     */
    size_t index_of(int32_t element) const {
        for (size_t i = 0; i < zaplneni; i += 1) {
            if (pole[i] == element) {
                return i;
            }
        }

        // Neco se rozbilo!
        throw std::logic_error("Prvek nebyl najit v poli.");
    }

    // Funkce pro "range-based for"
    int32_t* begin() const { return pole; }
    int32_t* end() const { return pole + zaplneni; }
};



/* ------------------------- TESTY ------------------------- */

void test() {
    Mnozina m1(16);
    Mnozina m2(16);

    // Mnozina << výraz
    m1 << 4 << 7 << 9 << 11 << 2 << -4 << -1 << 0;
    m2 << -13 << 11 << 0 << -2 << -9 << 10 << -1 << -8 << -8 << 9;

    // Mnozina()
    m1(8);
    m2(8);

    // Mnozina += Mnozina
    m1 += m2;
    m1(8);

    // Mnozina -= výraz
    m1 -= 11;
    m1(8);

    // Mnozina -= Mnozina
    m1 -= m2;
    m1(8);

    // +Mnozina
    std::cout << "Pocet prvku v mnozine 1: " << +m1 << std::endl;
    std::cout << "Pocet prvku v mnozine 2: " << +m2 << std::endl;
}



/* ------------------------- MAIN ------------------------- */

int main() {
    test();
    return 0;
}