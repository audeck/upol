#include <iostream>
#include <cstring>
#include <cstdlib>

using namespace std;

class Jmeno { 
    static Jmeno** pole;  // v .inc souboru je 'pole', v zadani jsou 'jmena' :^)
    static unsigned int rozsah, pocet, iter;
    const char* jm;

    public: 
        Jmeno(const char* name) {
            if (rozsah == 0) {
                reallocPole(5);
            } else if (rozsah == pocet) {
                reallocPole(rozsah * 2);
            }

            jm = strdup(name);  // pouziva malloc!
            pole[pocet] = this;
            pocet += 1;
        }

        ~Jmeno() {
            free((void*) jm);
        }

        const char* jmeno() const {
            return jm;
        }

        static bool zacatek() {
            iter = 0;
            return (pocet > 0);
        }

        static const Jmeno* dalsi() {
            iter += 1;
            return pole[iter - 1];
        }

        static bool jeDalsi() {
            return (iter < pocet);
        }

    private:
        static void reallocPole(unsigned int novy_rozsah) {
            Jmeno** nove_pole = new Jmeno*[novy_rozsah];

            /* Zkopiruj obsah stavajiciho pole do noveho a odstran ho (pokud uz existuje) */
            if (rozsah > 0) {
                memcpy(nove_pole, pole, rozsah * sizeof(Jmeno*));
                delete pole;
            }

            pole = nove_pole;
            rozsah = novy_rozsah;
        }
};
 
/* Inicializace statickych promennych */
Jmeno** Jmeno::pole = nullptr;
unsigned int Jmeno::rozsah = 0;
unsigned int Jmeno::pocet = 0;
unsigned int Jmeno::iter = 0;

int main() {
    typedef Jmeno J;

    new J("Jana"), new J("Petr"), new J("Eva"), new J("Jan"), new J("Irena"),
    new J("Alena"), new J("Pavel"), new J("Roman"), new J("Veronika"), new J("Jitka"),
    new J("Denisa");

    char h[]="Hana";
    new J(h);
    *h='?';

    if (Jmeno::zacatek()) {
        cout << "Jmena v objektech:" << endl;
        while (Jmeno::jeDalsi()) {
            cout << " - " << Jmeno::dalsi()->jmeno() << endl;
        }
    }

    return 0;
}
