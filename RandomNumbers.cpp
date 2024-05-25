#include <bits/stdc++.h>
#include <chrono>
using namespace std;

#define inf numeric_limits<double>::infinity()

int main() {
    unsigned seed = chrono::system_clock::now().time_since_epoch().count();
    mt19937 gen(seed);

    int n = 100;
    cout << n << endl;

    uniform_real_distribution<double> mantissa_dist(-10.0, 10.0);
    uniform_int_distribution<int> exponent_dist(-10, 11);

    for (int i = 0; i < n; ++i) {
        double mantissa = mantissa_dist(gen);
        int exponent = exponent_dist(gen);
        double result = mantissa * pow(10.0, exponent);

        cout << setprecision(16) << scientific;
        cout << result << endl;
    }


    return 0;
}
