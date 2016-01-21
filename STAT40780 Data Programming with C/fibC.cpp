//NAME: Brian Buckley
//STUDENT NUMBER: 14203480

#include <iostream>
using namespace std;


// Generates the first n fibonacci numbers and returns them in the vector vec

extern "C" {
    void fibC (int* n, int* vec) { 
        int ret[*n];
        
        cout << "fibonacci sequence of first " << *n << " numbers is: " << endl;
        for (int i = 0; i<*n; i++) {
            if (i < 2) {
                ret[i] = 1;
            } else {
                ret[i] = (i-1) + (i-2);
            }
            cout << ret[i] << " ";
        }
        
        vec = ret;
    }
}

