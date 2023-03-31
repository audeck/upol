#include <iostream>
#include <cstring>



/**
 * Trida pro prioritni frontu s generickymi typy. Implementuje
 * pridavani a odebirani. Frontu reprezentuje implicitnim polem,
 * ktere obsahuje objekty typu `Prvek` (viz. zdrojovy kod).
 * 
 * @tparam P typ pro reprezentaci priority; musi implementovat operator `<`
 * @tparam D typ pro reprezentaci dat; musi implementovat operator `==`
 * @tparam n velikost implicitniho pole fronty
 * 
 * @note Implicitni pole je na zasobniku.
 */
template<class P, class D, size_t n = 50>
class PFronta {
  protected:
    struct Prvek {
        P priorita;
        D data;
    };
    Prvek pole[n];
    size_t zaplneni = 0;

  public:
    /**
     * Prida do fronty prvek dvojice (priorita, data).
     * Pokud je fronta plna, nebo fronta prvek uz obsahuje,
     * neprida nic.
     * 
     * @param priorita objekt reprezentujici prioritu
     * @param data objekt reprezentujici data
     * @return int -1 pokud je fronta plna, 0 pokud fronta prvek obsahuje, jinak 1
     */
    int pridat(const P& priorita, const D& data) {
        // Fronta je plna
        if (zaplneni == n) return -1;

        // Fronta uz obsahuje Prvek(priorita, data)
        if (obsahuje(priorita, data)) return 0;

        // Pridej novy Prvek(priorita, data) do fronty
        pole[zaplneni].priorita = priorita;
        pole[zaplneni].data = data;
        zaplneni += 1;

        return 1;
    }

    /**
     * Odebere (nejstarsi) prvek s nejvetsi prioritou z fronty
     * a ulozi jeho data v `data`.
     * 
     * @param data reference pro vystup dat
     * @return `true` pokud byl jakykoliv prvek odebran, jinak `false`
     */
    bool odebrat(D& data) {
        // Fronta je prazdna
        if (zaplneni == 0) return false;

        // Index max prvku
        size_t max = 0;

        // Najdi max index
        for (int i = 1; i < zaplneni; i += 1) {
            //  !(pole[i].priorita < pole[max].priorita) <=> pole[i].priorita >= pole[max].priorita
            if (!(pole[i].priorita < pole[max].priorita)) {
                max = i;
            }
        }

        // "Vrat" data
        data = pole[max].data;

        // Odstran max prvek
        memmove(pole + max, pole + max + 1, (zaplneni - max) * sizeof(Prvek));
        zaplneni -= 1;

        return true;
    }

    /**
     * Vraci true pokud fronta obsahuje alespon
     * jeden prvek.
     * 
     * @return true pokud fronta obsahuje prvky, jinak false
     */
    inline bool has_next() const { return !(zaplneni == 0); }

  private:
    /**
     * Vraci true, pokud fronta obsahuje prvek s
     * prioritou `priorita` a daty `data`.
     * 
     * @param priorita priorita ke srovnani
     * @param data data ke srovnani
     * @return true pokud fronta obsahuje prvek(`priorita`, `data`),
     * @return jinak false 
     */
    bool obsahuje(const P& priorita, const D& data) const {
        for (int i = 0; i < zaplneni; i += 1) {
            P cur_prio = pole[i].priorita;
            D cur_data = pole[i].data;

            // !(cur_prio < priorita) && !(priorita < cur_prio) <=> cur_prio == priorita
            if (cur_data == data && !(cur_prio < priorita) && !(priorita < cur_prio)) {
                return true;
            }
        }

        return false;
    }
};



/** Struct pro jmeno. */
struct Jmeno {
    char jmeno[20];
    Jmeno() { }
    Jmeno(const char* j) {
        strcpy(jmeno, j);
    }
    bool operator == (const Jmeno& jine_jmeno) const {
        return strcmp(jmeno, jine_jmeno.jmeno) == 0;
    }
};



/** Struct pro prioritu. */
struct Priorita {
    int priorita;
    Priorita() { }
    Priorita(int p) : priorita(p) { }
    bool operator < (Priorita p) const {
        return priorita < p.priorita;
    }
};



int main() {
    // Fronta<short, char>
    PFronta<short, char> fronta1;

    fronta1.pridat(5, 'a');
    fronta1.pridat(3, 'd');
    fronta1.pridat(1, 'e');
    fronta1.pridat(5, 'b');
    fronta1.pridat(5, 'a');
    fronta1.pridat(4, 'c');

    char c;
    std::cout << "[PFronta<short, char>]:" << std::endl;
    while (fronta1.has_next()) {
        fronta1.odebrat(c);
        std::cout << c << " ";
    }
    std::cout << std::endl << std::endl;



    // Fronta<int, Jmeno>
    PFronta<int, Jmeno> fronta2;

    fronta2.pridat(5, Jmeno("Pavel"));
    fronta2.pridat(3, Jmeno("Jana"));
    fronta2.pridat(1, Jmeno("Eva"));
    fronta2.pridat(5, Jmeno("Irena"));
    fronta2.pridat(4, Jmeno("Pavla"));
    fronta2.pridat(1, Jmeno("Petr"));
    fronta2.pridat(3, Jmeno("Hana"));
    fronta2.pridat(1, Jmeno("Eva"));
    fronta2.pridat(7, Jmeno("Jan"));
    fronta2.pridat(5, Jmeno("Roman"));

    Jmeno j;
    std::cout << "[PFronta<int, Jmeno>]:" << std::endl;
    while (fronta2.has_next()) {
        fronta2.odebrat(j);
        std::cout << j.jmeno << " ";
    }
    std::cout << std::endl << std::endl;



    // Fronta<Priorita, Jmeno>
    PFronta<Priorita, Jmeno> fronta3;

    fronta3.pridat(Priorita(5), Jmeno("Pavel"));
    fronta3.pridat(Priorita(3), Jmeno("Jana"));
    fronta3.pridat(Priorita(1), Jmeno("Eva"));
    fronta3.pridat(Priorita(5), Jmeno("Irena"));
    fronta3.pridat(Priorita(4), Jmeno("Pavla"));
    fronta3.pridat(Priorita(1), Jmeno("Petr"));
    fronta3.pridat(Priorita(3), Jmeno("Hana"));
    fronta3.pridat(Priorita(1), Jmeno("Eva"));
    fronta3.pridat(Priorita(7), Jmeno("Jan"));
    fronta3.pridat(Priorita(5), Jmeno("Roman"));

    std::cout << "[PFronta<Priorita, Jmeno>]:" << std::endl;
    while (fronta3.has_next()) {
        fronta3.odebrat(j);
        std::cout << j.jmeno << " ";
    }
    std::cout << std::endl;



    return 0;
}
