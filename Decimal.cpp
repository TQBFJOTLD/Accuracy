#include <bits/stdc++.h>
using namespace std;

typedef long long LL;

const double eps = 1e-38;
const int N = 50;
int A[N], B[N], fa[N], fb[N], R[N], fr[N], D[N], fd[N], T[N], ft[N];

struct NumInfo {
    double value;
    int originalId;
    int exponent;
    double significand;

    NumInfo(double v, int idx) : value(v), originalId(idx) {
        significand = frexp(value, &exponent);
    }
};

bool is_fp16(double x) {
    return (x == 0.0) || (fabs(x) >= 6.10352e-5 && fabs(x) <= 65504.0);
}

void HalfMask(float &f)
{
    uint32_t word = *((uint32_t *)(&f));
    uint32_t sign = word & 0x80000000;
    uint32_t exp  = (word & 0x7f800000) >> 23;
    uint32_t frac = word & 0x007fffff;
        
    if (exp > 142 || ((exp == 142) && (frac & 0x00001fff))) {
        exp = 255;
        frac = 0;
    }
    else if (exp > 112) {
        frac &= 0x007fe000;
    }
    else if (exp > 102) {
        frac &= 0x007fe000 << (113 - exp);
    }
    else {
        exp = 0;
        frac = 0;
    }
    word = sign | (exp << 23) | frac;
    *((uint32_t *)(&f)) = word;
}

string encode(const vector<int>& ids, char precision) {
    ostringstream oss;
    oss << "{" << precision << ":";
    for (int i = 0; i < ids.size(); i ++) {
        if (i > 0) oss << ",";
        oss << ids[i];
    }
    oss << "}";
    return oss.str();
}

char setPrecision(double sum) {
    if (fabs((float)sum - sum) < eps) return 's';
    return 'd';               
}

pair<double, string> pairWise(const vector<double>& values, const vector<int>& ids) {
    if (ids.size() == 1) {
        return {values[ids[0] - 1], to_string(ids[0])};
    }
    if (ids.size() == 2) {
        double sum = values[ids[0] - 1] + values[ids[1] - 1];
        char precision = setPrecision(sum);
        string encoded = "{" + string(1, precision) + ":" + to_string(ids[0]) + "," + to_string(ids[1]) + "}";
        return {sum, encoded};
    }

    int mid = ids.size() / 2;
    vector<int> leftIds(ids.begin(), ids.begin() + mid);
    vector<int> rightIds(ids.begin() + mid, ids.end());

    auto leftResult = pairWise(values, leftIds);
    auto rightResult = pairWise(values, rightIds);

    double combinedSum = leftResult.first + rightResult.first;
    char combinedPrecision = setPrecision(combinedSum);
    string combinedEncoded = "{" + string(1, combinedPrecision) + ":" + leftResult.second + "," + rightResult.second + "}";

    return {combinedSum, combinedEncoded};
}

double eval(const vector<double>& numbers, string output) {
    stack<char> types;
    stack<double> results;
    stack<int> count;
    int cnt = 0; // number of digits in each bracket
    auto to_num = [&](double x, char c) -> double {
        if (c == 'd') return x;
        return static_cast<double>(static_cast<float>(x));
    };

    for (int i = 0; i < (int)output.size(); i ++) {
        auto c = output[i];
        if (c == '{' || c == ',' || c == ':') {
            continue;
        }
        else if (c == 'd' || c == 'h' || c == 's') {
            types.push(c);
        }
        else if (isdigit(c)) {
            int x = 0, j = i;
            while (j < (int)output.size() && isdigit(output[j])) {
                x = x * 10 + (output[j] - '0');
                j ++;
            }
            i = j - 1;
            char precision = types.top();
            results.push(to_num(numbers[x - 1], precision));
            cnt ++;
            if (output[j] == '}' || (j + 2 < (int)output.size() && output[j + 1] == '{')) {
                count.push(cnt);
                cnt = 0;
            }
        }
        else if (c == '}') {
            if (results.empty()) continue;
            double sum = 0.0;
            vector<double> tmp;
            int m = count.top(); count.pop();
            char precision = types.top(); types.pop();
            while(m --) {
                double t = results.top(); results.pop();
                tmp.emplace_back(to_num(t, precision));
            }
            for (int j = tmp.size() - 1; j >= 0; j --) sum += tmp[j];
            results.push(sum);
            if (count.size()) {
                m = count.top(); 
                count.pop();
                count.push(m + 1);
            }
            else {
                count.push(1);
                types.push(precision);
            }
            
        }
    }
    double sum = 0.0;
    vector<double> tmp;
    char precision = types.top(); types.pop();
    while(results.size()) {
        double t = results.top(); results.pop();
        tmp.emplace_back(to_num(t, precision));
    }
    for (int j = tmp.size() - 1; j >= 0; j --) sum += tmp[j];
    return sum;
}
/************************************************************
 *                   Read Numbers
 * **********************************************************/
void readNum(string& str, int *I, int *fi) {
    memset(I, 0, N * sizeof(int));
    memset(fi, 0, N * sizeof(int));
    int exp = 0;
    int signExp = 1, sign = 1;
    int posI = N - 1, posF = 0;
    bool frac = false;
    if (str.find('e') != string::npos) {
        int p = str.find('e');
        // cout << "p = " << p << endl;
        int x = 0;
        if (str[p + 1] == '+') {
            signExp = 1;
            p ++;
        }
        else if (str[p + 1] == '-') {
            p ++;
            signExp = -1;
        }

        for (int i = p + 1; i < str.size(); i ++) {
            x = x * 10 + str[i] - '0';
        }
        exp = signExp * x;
    }
    else {
        if (str.find('.') != string::npos) {
            int dot = str.find('.');
            posI = N - dot;
            if (!isdigit(str[0])) posI ++;
        }
        else {
            posI = N - (int)str.size();
            if (!isdigit(str[0])) posI ++;
        }
    }

    if (exp > 0) posI -= exp;
    if (exp < 0) posF -= (exp + 1);

    int start = 0;
    if (str[0] == '+') start ++;
    else if (str[0] == '-') {
        start ++;
        sign = -1;
        I[0] = 1;
    }

    int dot = str.find('.');
    // cout << str << ' ' << dot << ' ' << exp << ' ' << posF << ' ' << posI << endl;
    for (int i = start; i < str.size(); i ++) {
        if (dot != -1 && i > dot + exp) {
            frac = true;
        }
        if (str[i] == '.') continue;
        if (str[i] == '+' || str[i] == '-' || str[i] == 'e') break;
        if (frac) {
            fi[posF] = str[i] - '0';
            posF ++;
        }
        else {
            I[posI] = str[i] - '0';
            posI ++;
        }
    }

     // cout << str << ", exp: " << exp << ", posF: " << posF << ", posI: " << posI << ", signExp: " << signExp << ", sign: " << sign << endl;
}

int cmp(int *A, int *fa, int *B, int *fb) {
    for (int i = 1; i < N; i ++) {
        if (A[i] != B[i]) {
            if (A[i] > B[i]) return 1;
            else return -1;
        }
    }

    for (int i = 0; i < N; i ++) {
        if (fa[i] != fb[i]) {
            if (fa[i] > fb[i]) return 1;
            else return -1;
        }
    }

    return 0;
}

void sub(int *A, int *fa, int *B, int *fb) {
    int borrow = 0;
    for (int i = N - 1; i >= 0; i--) {
        fa[i] -= (fb[i] + borrow);
        if (fa[i] < 0) {
            borrow = 1;
            fa[i] += 10;
        } else {
            borrow = 0;
        }
    }

    for (int i = N - 1; i; i--) {
        A[i] -= (B[i] + borrow);
        if (A[i] < 0) {
            borrow = 1;
            A[i] += 10;
        } else {
            borrow = 0;
        }
    }
}

bool add(int *A, int *fa, int *B, int *fb) {
    int carry = 0;
    bool swapped = false;
    if (A[0] == B[0]) {
        for (int i = N - 1; i >= 0; i--) {
            fa[i] += fb[i];
            if (i > 0) fa[i - 1] += fa[i] / 10;
            else carry = fa[i] / 10;
            fa[i] %= 10;
        }

        // cout << "carry = " << carry << endl;
        for (int i = N - 1; i; i--) {
            if (i == N - 1) A[i] += B[i] + carry;
            else A[i] += B[i];
            if (i > 1) A[i - 1] += A[i] / 10;
            A[i] %= 10;
        }
    }
    else {
        if (A[0] == 1 && B[0] == 0) {
            if (cmp(B, fb, A, fa) == 1) {
                swap(A, B);
                swap(fa, fb);
                sub(A, fa, B, fb);
                swapped = true;
            }
            else if (cmp(B, fb, A, fa) == -1) {
                sub(A, fa, B, fb);
            }
            else {
                memset(A, 0, N * sizeof(int));
            }
        }
        else if (A[0] == 0 && B[0] == 1){
            if (cmp(A, fa, B, fb) == 1) {
                sub(A, fa, B, fb);
            }
            else if (cmp(A, fa, B, fb) == -1){
                swap(A, B);
                swap(fa, fb);
                sub(A, fa, B, fb);
                swapped = true;
            }
            else {
                memset(A, 0, N * sizeof(int));
            }
        }
    }
    return swapped;
}

string dtos(double x) {
    ostringstream oss;
    oss << fixed << setprecision(20) << x;
    cout << "converted number: " << x << ", " << oss.str() << endl;
    return oss.str();
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);
    cout << setprecision(20);

    int n;
    cin >> n;
    vector<double> numbers;
    vector<int> ids;
    string res;
    double recursive_sum = 0;

    string str1, str2;
    cin >> str1;
    numbers.emplace_back(stod(str1));
    readNum(str1, A, fa);
    recursive_sum += numbers[0];

    for (int i = 0; i < N; i ++ ) cout << A[i]; cout << endl;
    for (int i = 0; i < N; i ++ ) cout << fa[i]; cout << endl;
    cout << endl;


    for (int i = 1; i <= n; i ++) ids.emplace_back(i);

    for (int i = 0; i < n - 1; ++i) {
        cin >> str2;
        numbers.emplace_back(stod(str2));
        readNum(str2, B, fb);
        if(add(A, fa, B, fb)) {
            swap(A, B);
            swap(fa, fb);
        }
        recursive_sum += numbers.back();
    }

    // cout << "Sum: " << endl;
    // for (int i = 0; i < N; i ++ ) cout << A[i]; cout << endl;
    // for (int i = 0; i < N; i ++ ) cout << fa[i]; cout << endl;
    // cout << endl << endl;

    string resSimple = "{d:" + encode(ids, 'd') + "}";
    pair<double, string> resNonSort = pairWise(numbers, ids);
    
    cout << "recursive sum: " << recursive_sum << endl;
    

    sort(ids.begin(), ids.end(), [&](int p, int q) {
            return fabs(numbers[p - 1]) < fabs(numbers[q - 1]);
        });

    string resSortAsc = "{d:" + encode(ids, 'd') + "}";

    sort(ids.begin(), ids.end(), [&](int p, int q) {
        return fabs(numbers[p - 1]) > fabs(numbers[q - 1]);
    });

    pair<double, string> resSortDsc = pairWise(numbers, ids);


    bool has_first = false;
    int cnt = 0;
    string resPerfect;
    for (int i = 0; i < 30; i ++) {
        if (n >> i & 1) {
            int sz = (1 << i);
            vector<int> bk{ids.begin() + cnt, ids.begin() + cnt + sz};
            if (has_first) resPerfect += ",";
            resPerfect += pairWise(numbers, bk).second;
            has_first = true;
            cnt += sz;
        }
    }
    resPerfect = "{d:" + resPerfect + "}";

    double scoreSimple = eval(numbers,  "{d:" + resSimple + "}");
    double scoreNonSort = eval(numbers, "{d:" + resNonSort.second + "}");
    double scoreSortAsc = eval(numbers, "{d:" + resSortAsc + "}");
    double scoreSortDsc = eval(numbers, "{d:" + resSortDsc.second + "}");
    double scorePerfect = eval(numbers, "{d:" + resPerfect + "}");
    vector<double> scores{scoreSimple, scoreNonSort, scoreSortAsc, scoreSortDsc, scorePerfect};
    vector<string> ansStr{resSimple, resNonSort.second, resSortAsc, resSortDsc.second, resPerfect};
/***********************************************************************************
 *                                   Compare
 * *********************************************************************************/
    cout << "Real sum: " << endl;
    for (int i = 0; i < N; i ++ ) cout << A[i]; cout << endl;
    for (int i = 0; i < N; i ++ ) cout << fa[i]; cout << endl;
    cout << endl;

    memset(D, 0x3f, sizeof D);
    memset(fd, 0x3f, sizeof fd);
    D[0] = 0;
    int optimal = 0;
    for (int i = 0; i < scores.size(); i ++) {
        string tmp = dtos(scores[i]);
        readNum(tmp, R, fr);
        cout << "Current sum: " << tmp << ' ' << ansStr[i] << endl;
        for (int i = 0; i < N; i ++ ) cout << R[i]; cout << endl;
        for (int i = 0; i < N; i ++ ) cout << fr[i]; cout << endl;

        if (cmp(R, fr, A, fa) >= 0) {
            sub(R, fr, A, fa);
            R[0] = 0;
        }
        else {
            memcpy(T, A, sizeof A);
            memcpy(ft, fa, sizeof fa);
            sub(A, fa, R, fr);
            A[0] = 0;
            memcpy(R, A, sizeof A);
            memcpy(fr, fa, sizeof fa);
            memcpy(A, T, sizeof T);
            memcpy(fa, ft, sizeof ft);
        }

        cout << "Current diff: " << endl;
        for (int i = 0; i < N; i ++ ) cout << R[i]; cout << endl;
        for (int i = 0; i < N; i ++ ) cout << fr[i]; cout << endl;
        cout << endl;
    
        if (cmp(R, fr, D, fd) == -1) {
            optimal = i;
            memcpy(D, R, sizeof R);
            memcpy(fd, fr, sizeof fr);
        }
    }
    for (int i = 0; i < scores.size(); i ++) cout << scores[i] << ' '; cout << endl;
    cout << "optimal id : " << optimal << endl;

    if (optimal == 0) cout << resSimple << endl;
    else if (optimal == 1) cout << resNonSort.second << endl;
    else if (optimal == 2) cout << resSortAsc << endl;
    else if (optimal == 3) cout << resSortDsc.second << endl;
    else cout << resPerfect << endl;
     
    return 0;
}
