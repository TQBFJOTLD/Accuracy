#include <bits/stdc++.h>
using namespace std;

const int N = 30;

void readNum(const string& str, int* I, int* fi, int& sign) {
    memset(I, 0, sizeof(int) * N);
    memset(fi, 0, sizeof(int) * N);
    int exp = 0, posI = N - 1, posF = 0;
    bool frac = false, signExp = 1;

    if (str.find('e') != string::npos) {
        size_t p = str.find('e');
        int x = 0, offset = (str[p + 1] == '+' || str[p + 1] == '-') ? 2 : 1;
        signExp = (str[p + 1] == '-') ? -1 : 1;

        for (size_t i = p + offset; i < str.size(); i++)
            x = x * 10 + (str[i] - '0');
        exp = x * signExp;
    }

    int start = (str[0] == '+' || str[0] == '-') ? 1 : 0;
    sign = (str[0] == '-') ? -1 : 1;

    for (size_t i = start; i < str.size(); i++) {
        if (str[i] == '.' || str[i] == 'e') {
            frac = (str[i] == '.');
            if (str[i] == 'e') break;
            continue;
        }

        if (frac && exp >= 0) {
            if (posF < N) fi[posF++] = str[i] - '0';
        } else if (!frac && exp <= 0) {
            if (posI >= 0) I[posI--] = str[i] - '0';
        }
    }

    // Adjusting for exponent on the integer part
    while (exp-- > 0 && posI >= 0) I[posI--] = 0;
    while (exp++ < 0 && posF < N) fi[posF++] = 0;
}

void addOrSub(int *A, int *fa, int *B, int *fb, int signA, int signB) {
    if (signA == signB) {
        int carry = 0;
        for (int i = N - 1; i >= 0; i--) {
            fa[i] += fb[i] + carry;
            carry = fa[i] / 10;
            fa[i] %= 10;
        }

        for (int i = N - 1; i >= 0; i--) {
            A[i] += B[i] + carry;
            carry = A[i] / 10;
            A[i] %= 10;
        }
    } else {
        int borrow = 0;
        for (int i = N - 1; i >= 0; i--) {
            fa[i] -= (fb[i] + borrow);
            borrow = (fa[i] < 0) ? 1 : 0;
            if (fa[i] < 0) fa[i] += 10;
        }

        for (int i = N - 1; i >= 0; i--) {
            A[i] -= (B[i] + borrow);
            borrow = (A[i] < 0) ? 1 : 0;
            if (A[i] < 0) A[i] += 10;
        }
    }
}

int main() {
    int n, signA, signB;
    string str1, str2;
    int A[N], B[N], fa[N], fb[N];

    cin >> n >> str1;
    readNum(str1, A, fa, signA);
    for (int k = 0; k < n - 1; k++) {
        cin >> str2;
        readNum(str2, B, fb, signB);
        addOrSub(A, fa, B, fb, signA, signB);
    }

    cout << "Final result (integer part): ";
    for (int i = 0; i < N; i++) cout << A[i];
    cout << endl;

    cout << "Final result (fractional part): ";
    for (int i = 0; i < N; i++) cout << fa[i];
    cout << endl;

    return 0;
}
