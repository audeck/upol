#include <iostream>
#include <iomanip>
#include <cmath>

using namespace std;

class Cm {
    double value;
    double conversion_value = 1 / 2.54f;

    public: 
        Cm(double v) { value = v; }
        Cm(int v) { value = static_cast<double>(v); }

        double cm() { return value; }
        double palec() { return value * conversion_value; }

        double rozdil(Cm& other_value);
        double rozdil(class Palec& other_value);
};

class Palec {
    double value;
    double conversion_value = 2.54f;

    public: 
        Palec(double v) { value = v; }
        Palec(int v) { value = static_cast<double>(v); }

        double cm() { return value * conversion_value; }
        double palec() { return value; }
        
        double rozdil(class Cm& other_value);
        double rozdil(Palec& other_value);
};

double Cm::rozdil(Cm& other_value) { return fabs(cm() - other_value.cm()); }
double Cm::rozdil(Palec& other_value) { return fabs(palec() - other_value.palec()); }
double Palec::rozdil(Cm& other_value) { return fabs(cm() - other_value.cm()); }
double Palec::rozdil(Palec& other_value) { return fabs(palec() - other_value.palec()); }

int main() {
    Cm a(2), b(1.5);
    Palec u(1), v(1.5);

    cout            << "a="   << a.cm()      << " cm";
    cout << setw(7) << "b="   << b.palec()   << " palce";
    cout << setw(6) << "b-a=" << b.rozdil(a) << " cm";
    cout << setw(9) << "a-u=" << a.rozdil(u) << " palce\n";

    cout             << "u="   << u.cm()      << " cm";
    cout << setw(4)  << "v="   << v.palec()   << " palce";
    cout << setw(11) << "u-v=" << u.rozdil(v) << " palce";
    cout << setw(6)  << "u-b=" << u.rozdil(b) << " cm\n" << flush;
}