#include <iostream>
#include <iomanip>
using namespace std;



void AsciiTable() {
    char current;

    // Top edge
    cout << string(89, '-') << endl;

    // Table generation
    for (int i = 0; i < 12; i += 1) {
        for (int j = 0; j < 8; j += 1) {
            current = 32 + j + (i * 8);

            cout << "|" << setw(4) << dec << (int) current << " " 
                << uppercase << hex << (int) current << " ";

            if (current == 127) {
                cout << " ";
            } else {
                cout << current;
            }

            cout << " ";
        }

        cout << "|" << endl;
    }

    // Bottom edge
    cout << string(89, '-') << endl;
}



int main() {
    AsciiTable();
    return 0;
}
