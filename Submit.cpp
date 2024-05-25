#include <bits/stdc++.h>
#include <future>
#include <chrono>
using namespace std;

const int M = 55;
unordered_map<int, char> mp, magnitude;
double se;

constexpr double timeBonus    =  1.0e0;
constexpr double power        =  2.0e0;
constexpr double halfTime     =  1.0e0;
constexpr double floatTime    =  2.0e0;
constexpr double doubleTime   =  4.0e0;
constexpr double maxPrecision = 53.0e0;
constexpr double mantissaPow = 0.05;
constexpr double maxAs = 1.0e-20;

constexpr double penaltyScaling       = 10.0e0; //scaling factor
constexpr unsigned int minPenalty     =  0; //minimal unscaled penalty
constexpr unsigned int maxPenalty     = 0; //maximal unscaled penalty, default 17
double penaltyCounter           = 0; //inital counter
constexpr double penaltyBarrier = 10'000; //penalty step
float currentPenaltyFee               = 0.0e0; //penalty storage
constexpr float penaltyFeeScaling     = 50.0e-2; //penalty fee step

//Register "size" to penalize for long jumps in the sequence
constexpr uint32_t regSize=16;

// public part begin

//simulated fp16
class Float16{
    static const uint32_t mantissaShift = 42;
    static const uint32_t expShiftMid   = 56;
    static const uint32_t expShiftOut   = 52;
    double dValue_;

public:    
    Float16(double in) : dValue_(in) {
        uint64_t utmp;
        memcpy(&utmp, &dValue_, sizeof utmp);
        //zeroing mantissa bits starting from 11th (this is NOT rounding)
        utmp = utmp >> mantissaShift;
        utmp = utmp << mantissaShift;
        //setting masks for 5-bit exponent extraction out of 11-bit one
        const uint64_t maskExpMid = (63llu << expShiftMid);
        const uint64_t maskExpOut = (15llu << expShiftOut);
        const uint64_t maskExpLead = (1llu << 62);
        const uint64_t maskMantissaD = (1llu << 63) + maskExpLead + maskExpMid + maskExpOut;
        if (utmp & maskExpLead) {// checking leading bit, suspect overflow
            if (utmp & maskExpMid) { //Detected overflow if at least 1 bit is non-zero
                //Assign Inf with proper sign
                utmp = utmp | maskExpMid; //setting 1s in the middle 6 bits of of exponent
                utmp = utmp & maskMantissaD; //zeroing mantissa irrelative of original values to prevent NaN
                utmp = utmp | maskExpOut; //setting 1s in the last 4 bits of exponent
            }
        } else { //checking small numbers according to exponent range
            if ((utmp & maskExpMid) != maskExpMid) { //Detected underflow if at least 1 bit is 0
                utmp = 0;
            }
        }
        memcpy(&dValue_, &utmp, sizeof utmp);
    }

    Float16() : dValue_(0) {}

    Float16& operator=(const Float16& rhs) {
        this->dValue_ = rhs.dValue_;
        return *this;
    }

    Float16& operator=(const double& rhs) {
        this->dValue_ = rhs;
        uint64_t utmp;
        memcpy(&utmp, &dValue_, sizeof utmp);
        utmp = utmp >> mantissaShift;
        utmp = utmp << mantissaShift;
        memcpy(&dValue_, &utmp, sizeof utmp);
        return *this;
    }

    friend Float16 operator+(const Float16& lhs, const Float16& rhs) {
        double tmp = lhs.dValue_ + rhs.dValue_;
        return Float16(tmp);
    }

    float convert2Float() { return static_cast<float>(dValue_); }
    double convert2Double() { return dValue_; }
};

// Calculate the fp64 sum sequence like {d:1,2,3}
double calculateFp64(stack<double>& nums) {
    volatile double currResultDouble = 0;
    while (!nums.empty()) {
        currResultDouble += nums.top();
        nums.pop();
    }
    return currResultDouble;
}

// Calculate the fp32 sum sequence like {s:1,2,3}
double calculateFp32(stack<double>& nums) {
    float currResultSingle = 0;
    while (!nums.empty()) {
        currResultSingle += static_cast<float>(nums.top());
        nums.pop();
    }
    return static_cast<double>(currResultSingle);
}

// Calculate the fp16 sum sequence like {h:1,2,3}
double calculateFp16(stack<double>& nums) {
    Float16 currResultHalf(0.);
    while (!nums.empty()) {
        currResultHalf = currResultHalf + Float16(nums.top());
        nums.pop();
    }
    return currResultHalf.convert2Double();
}

// public part end

double randomPenalty() { //penalty for "memory jumps"
    
    penaltyCounter += 1.0;
    double currentPenaltyFee=(penaltyCounter / penaltyBarrier)*penaltyFeeScaling;
    return currentPenaltyFee;
}

//"Exact" answer based on Kahan summation
double kahan_sum(const vector<double>& vec) {
    long double trueSum=0, corr=0;
    vector<double> dvtmp=vec;
    sort(dvtmp.begin(),dvtmp.end(), [](const double x, const double y) {
        return fabs(x) < fabs(y);
    });
    for (auto i : dvtmp) {
        volatile long double y = static_cast<long double>(i) - corr;
        volatile long double t = trueSum + y;
        corr = (t - trueSum) - y;
        trueSum = t;
    }
    return (double)trueSum;
}

//Mantissa-based score multiplier
double same_mantissa_count(const double calculatedSum, const double expectedSum) {
    if (isinf(calculatedSum) || isinf(expectedSum)) return (double)1.0e20; //worst case
    if (isnan(calculatedSum) || isnan(expectedSum)) return (double)1.0e20; //worst case
    if (calculatedSum == expectedSum) return 1.0e-20; //best case
    return std::max(std::fabs(calculatedSum-expectedSum)/std::max(std::fabs(expectedSum), 1.0e-200), 1.0e-20);
}

//Calculate total score for the solution
double score_calulator(const float exeTime, const double mantissaBits) {
    const double maxScore = 10.0 * sqrt(timeBonus / halfTime + 0.5) / pow(maxAs, mantissaPow); //max possible score
    
    double newExeTime = timeBonus/(exeTime + 0.5);
    double score = 10.0 * sqrt(newExeTime) / pow(mantissaBits, mantissaPow);
    if (std::isnan(score) || std::isinf(score)) score=0.0; //assigne lowest possible score
    
    //Printing information for reference
    // cout<<"Current solution efficiency is "<<score/maxScore*100.0e0<<"% "<<endl;
    return score;
}

bool isExample = false;

//Interpreter and simulator of sum execution
pair<float,double> parse_and_simulate(const string& s, const vector<double>& vec) {
    stack<char> ops;
    stack<double> nums;

    vector< bool > vis(vec.size(), false);
    
    vector<uint32_t> reg;
    double penalty=0;
    reg.resize(regSize);
    uint32_t regPos=0;

    double excution_time = 0.0e0;
    double answer = 0.0e0;
    uint32_t nCounted=0;

    int slen = s.length();
    char curr_op = ' ';
    for (int i = 0; i < slen; i++) {
        if (s[i] == '{') {
            ops.push('{');
            ops.push(s[i + 1]);
            curr_op = s[i + 1];
            i += 2;
        } else if (isdigit(s[i])) {
            int j= i + 1;
            while (j < slen && isdigit(s[j])) ++j;

            int index;
            index = stoi(s.substr(i, j - i)) - 1;

            vis[index] = true;

            nums.push(vec[index]);
            i = j - 1;
            
            //Evaluating current penalty for long jumps
            if (regPos >= regSize) { //check if register is full
                for (uint32_t i=1; i<regSize; i++) {
                    if ((uint32_t)abs((int)reg[0]-(int)reg[i]) >= regSize) {
                        //penalty+=penaltyStep;
                        penalty+=randomPenalty(); //penalize randomly for jumps wrt to first index in register
                    }
                }
                regPos=0;
            }
            //Append next index to register
            reg[regPos]=index;
            regPos++;
        }
        else if (s[i] == ',') {

            ops.push(curr_op);
            nCounted++;
            if (curr_op == 'd') {
                excution_time += doubleTime;
            }
            else if (curr_op == 's') {
                excution_time += floatTime;
            }
            else if (curr_op == 'h') { // 'h'
                excution_time += halfTime;
            }
        }
        else if (s[i] == '}') {
            stack<double> tmp_s;
            curr_op = ops.top();
            while (ops.top() != '{') {
                tmp_s.push(nums.top());
                ops.pop();
                nums.pop();
            }

            if (curr_op == 'd') {
                nums.push(calculateFp64(tmp_s));
            }
            else if (curr_op == 's') {
                nums.push(calculateFp32(tmp_s));
            }
            else { // h
                nums.push(calculateFp16(tmp_s));
            }

            ops.pop();
            if (!ops.empty()) {
                curr_op = ops.top();
            } else {
                curr_op = ' ';
            }
        }
    }
    answer = nums.top(); // Calculated sum based on provided coding sequence

    double ref = se; //Expected answer based on Kahan summation
    double mantissa_bits = same_mantissa_count(answer, ref); // Accuracy score
    // std::cout << "same mantissa score is " << mantissa_bits << std::endl;
    for (uint32_t i=1; i<regPos; i++) { // Process remaining numbers in register
        if ((uint32_t)abs((int)reg[0]-(int)reg[i]) >= regSize) {
            penalty+=randomPenalty();
        }
    }
    // std::cout << " Penalty " << penalty << std::endl;
    // std::cout << " Execution time " << excution_time << std::endl;
    excution_time+=penalty; //Appending penalty to execution time
    if (nCounted > vec.size()) nCounted=vec.size(); //Prevent summation of infinite number of zeros in lowest precision
    excution_time/=nCounted; //Calculate average execution per single sum operation including jump penalty
    // std::cout << " Average execution time " << excution_time << std::endl;
    return make_pair(excution_time, mantissa_bits);
}

bool checkType (char c) {
    if (c == 'h' || c == 's' || c == 'd') return true;
}

struct pairInfo {
    string ans;
    double sum;
    char type;
};

struct NumInfo {
    double value;
    int originalId;
    int exponent;
    bool sign;
    bitset<M> significand;

    NumInfo() : value(0), originalId(-1), exponent(-1024), sign(false) {}
    NumInfo(double v, int idx) : value(v), originalId(idx) {
        if (v == 0) return;
        sign = (v < 0);
        v = abs(v);
        double frac = frexp(v, &exponent);
        frac *= 2;
        if (frac >= 1) frac -= 1;
        exponent--;

        for (int i = 0; i < M; i++) {
            frac *= 2;
            if (frac >= 1) {
                significand.set(i);
                frac -= 1;
            }
        }
    }
};


char checkBitsPrecision(const NumInfo& ni) {
    auto s = ni.significand;
    if (ni.value > 1e300) return 'd';
    if (ni.exponent >= -14 && ni.exponent <= 15) {
        auto ts = s;
        ts >>= 10;
        if (!ts.any()) return 'h';
    }
    if (ni.exponent >= -126 && ni.exponent <= 127) {
        s >>= 23;
        if (!s.any()) return 's';
    }
    return 'd';
}


char checkBitsMagnitude(const NumInfo& ni) {
    if (ni.exponent <= 15) {
        return 'h';
    }
    else if (ni.exponent <= 127) {
        return 's';
    }
    return 'd';
}

string encode(const vector<int>& ids, char precision) {
    ostringstream oss;
    oss << "{" << precision << ":";
    bool flag = false;
    for (int i = 0; i < ids.size(); i ++) {
        if (flag) oss << ",";
        oss << ids[i];
        flag = true;
    }
    oss << "}";
    return oss.str();
}

vector<string> ex(vector<double>& numbers, vector<int>& ids) {
    vector<string> ans;
    for (int i = 0; i < ids.size(); ) {
        int j = i;
        double sum = 0;
        while (j < ids.size() && mp[ids[j]] == mp[ids[i]]) {
            if (mp[ids[j]] != checkBitsPrecision(NumInfo(sum + numbers[ids[j] - 1], -1))) break;
            sum += numbers[ids[j] - 1];
            j ++;
        }
        vector<int> tmp(ids.begin() + i, ids.begin() + max(i + 1, j));
        string s = encode(tmp, mp[ids[i]]);
        ans.emplace_back(s);
        i = max(i + 1, j);
    }
    return ans;
}

string encode2(vector<double>& numbers, vector<int>& ids) {
    vector<string> ans = ex(numbers, ids);
    string res;
    double sum = 0;

    for (int i = 0; i < ids.size(); i ++) sum += numbers[ids[i] - 1];
    for (int i = 0; i < ans.size(); i ++) {
        res += ans[i];
        if (i != ans.size() - 1) res += ",";
    }

    char c = checkBitsPrecision(NumInfo(sum, -1));
    res = "{" + string(1, c) + ":" + res + "}";
    return res;
}

//ans sum type
pairInfo pairWise(const vector<double>& values, const vector<int>& ids, char type) {
    if (ids.size() == 1) {
         return {"{" + string(1, mp[ids[0]]) + ":" +  to_string(ids[0]) + "}", values[ids[0]], mp[ids[0]]};
    }
    if (ids.size() == 2) {
        double sum = values[ids[0] - 1] + values[ids[1] - 1];
        char precision = 'd';
        if (type != '#') {
            precision = type;
        }
        else {
            if (mp[ids[0]] != 'd' && mp[ids[1]] != 'd') {
                if (mp[ids[0]] == 'h' && mp[ids[1]] == 'h') {
                    if (checkBitsPrecision(NumInfo(sum, -1)) == 'h') precision = 'h';
                    else if (checkBitsPrecision(NumInfo(sum, -1)) == 's') precision = 's';
                }
                else {
                    if (checkBitsPrecision(NumInfo(sum, -1)) == 's') precision = 's';
                }
            }
        }

        string encoded = "{" + string(1, precision) + ":" + pairWise(values, {ids[0]}, '#').ans + "," + pairWise(values, {ids[1]}, '#').ans + "}";
        return {encoded, sum, precision};
    }

    int mid = ids.size() / 2;
    vector<int> leftIds(ids.begin(), ids.begin() + mid);
    vector<int> rightIds(ids.begin() + mid, ids.end());

    auto leftResult = pairWise(values, leftIds, type);
    auto rightResult = pairWise(values, rightIds, type);
    
    char lp = leftResult.type;
    char rp = rightResult.type;
    double combinedSum = leftResult.sum + rightResult.sum;
    char combinedPrecision = 'd';
    if (!checkType(lp) || !checkType(rp)) combinedPrecision = 'd';
    else {
        if (lp != 'd' && rp != 'd') {
            if (lp == 'h' && rp == 'h') {
                if (checkBitsPrecision(NumInfo(combinedSum, -1)) == 'h') combinedPrecision = 'h';
                else if (checkBitsPrecision(NumInfo(combinedSum, -1)) == 's') combinedPrecision = 's';
            }
            else {
                if (checkBitsPrecision(NumInfo(combinedSum, -1)) == 's') combinedPrecision = 's';
            }
        }
        else combinedPrecision = 'd';
    }

    if (type != '#') combinedPrecision = type;
    string combinedEncoded = "{" + string(1, combinedPrecision) + ":" + leftResult.ans + "," + rightResult.ans + "}";

    return {combinedEncoded, combinedSum, combinedPrecision};
}

char setPrecision(double sum) {
    return checkBitsPrecision(NumInfo(sum, -1));
}
 
string encodeGroups(const vector<string>& groups, char precision) {
    ostringstream oss;
    oss << "{" << precision << ":";
    for (int i = 0; i < groups.size(); i ++) {
        if (i) oss << ",";
        oss << groups[i];
    }
    oss << "}";
    return oss.str();
}
 
string kGroup(const vector<double>& values, const vector<int>& ids, char precision, int k) {
    vector<string> subGroups;
    int numGroups = (ids.size() + k - 1) / k;
    subGroups.reserve(numGroups);
    
    vector<double> subSum;
    for (int i = 0; i < numGroups; i ++) {
        int start = i * k;
        int end = min(start + k, (int)ids.size());
        vector<int> subGroup(ids.begin() + start, ids.begin() + end);
        char precision = 'd';
        subGroups.emplace_back(encode(subGroup, precision));
    }
 
    if (subGroups.size() > k) {
        vector<string> res;
        res.reserve((numGroups + k - 1) / k);
        for (int i = 0; i < numGroups; i += k) {
            int start = i;
            int end = min(start + k, (int)subGroups.size());
            vector<string> subGroup(subGroups.begin() + start, subGroups.begin() + end);
            char precision = 'd';
            res.emplace_back(encodeGroups(subGroup, precision));
        }
        return encodeGroups(res, precision);
    }
    else {
        return encodeGroups(subGroups, precision);
    }
}

double calculateScore(vector<double>& values, string &ans) {
    pair<float,double> time_and_mantissabits = parse_and_simulate(ans, values);
    double score = score_calulator(time_and_mantissabits.first, time_and_mantissabits.second);
    return score;
}

double error(double a, double b) {
    double s = a + b;
    double e = b - (s - a);
    return e;
}

vector<int> pSumOrdering(vector<double>& values, vector<int>& ids) {
    int n = ids.size();
    vector<int> res;

    double x0 = 0;
    unordered_map<int, bool> used;
    for (int i = 0; i < ids.size(); i ++) {
        int cur = -1;
        for (auto j: ids) {
            double diff = 1e300;
            if (used.count(j)) continue;
            if (fabs(error(x0, values[j])) < diff) {
                diff = fabs(error(x0, values[j]));
                cur = j;
            }
            if (cur != -1) {
                used[cur] = true;
                x0 += values[cur];
                res.push_back(cur);
            }
        }

    }
    // cout << "Ordered! " << res.size() << ' ' << n << endl;

    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(0);

    cout << fixed << setprecision(16);

    int n;
    cin >> n;
    vector<double> numbers;
    vector<NumInfo> nums;
    vector<int> ids;
    for (int i = 1; i <= n; i ++) ids.emplace_back(i);
    double recursive_sum = 0, abs_sum = 0;

    for (int i = 0; i < n; i ++) {
        double x;
        cin >> x;
        nums.emplace_back(x, i + 1);
        mp[i + 1] = checkBitsPrecision(nums[i]);
        numbers.emplace_back(x);
        recursive_sum += x;
        abs_sum += fabs(x);
    }
   
/**********************************************************************************
 *                             Special Judge
 * ********************************************************************************/
    se = kahan_sum(numbers);
    string ans;
    vector<int> ids2 = ids;
    vector<int> ids3 = ids;

    vector<double> abs_numbers(numbers.size());
    transform(numbers.begin(), numbers.end(), abs_numbers.begin(), [](double x) { return fabs(x); });

    sort(ids2.begin(), ids2.end(), [&](int p, int q) {
        return abs_numbers[p - 1] > abs_numbers[q - 1];
    });

    ids3 = ids2;
    reverse(ids2.begin(), ids2.end());
    
    ans = encode2(numbers, ids3);
    string ans2 = encode2(numbers, ids2);
    string ans3 = pairWise(numbers, ids, '#').ans;
    string ans4 = encode2(numbers, ids);

    double mx_score = 0;
    double score = calculateScore(numbers, ans);
    double score2 = calculateScore(numbers, ans2);
    double score3 = calculateScore(numbers, ans3);
    double score4 = calculateScore(numbers, ans4);

    vector<double> scores{score, score2, score3, score4};
    mx_score = *max_element(scores.begin(), scores.end());
    int index = max_element(scores.begin(), scores.end()) - scores.begin();

    if (index == 1) ans = move(ans2);
    else if (index == 2) ans = move(ans3);
    else if (index == 3) ans = move(ans4);


    if (mx_score < 47 && n < 650000) {
        for (int k = 13, j = 0; k <= max(min(n, 220), (int)sqrt(n)) && j <= 12.5e6 / n; k += 1, j ++) {
            string tmp = move(kGroup(numbers, ids, 'd', k));
            double ts = calculateScore(numbers, tmp);
            if (ts > mx_score) {
                mx_score = ts;
                ans = move(tmp);        
            }
            
            if (mx_score > 47) break;
        }
    }   

    if (n <= 5000 && score < 47) {
        unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
        std::mt19937 gen(seed);
        int cnt = 7000;

        while (cnt --) {
            shuffle(ids.begin(), ids.end(), gen);
            string tmp = move(encode2(numbers, ids));
            double ts = calculateScore(numbers, tmp);
            if (ts > mx_score) {
                mx_score = ts;
                ans = move(tmp);        
            }
            if (mx_score > 47) break;
        }
    
    }

    if (n <= 1e5) {
        priority_queue<pair<double, int>, vector<pair<double, int>>, greater<pair<double, int>>> heap;
        for (int i = 1; i <= n; i ++) {
            heap.push({fabs(numbers[i - 1]), i});
        }

        vector<int> ids4;
        while (heap.size() > 1) {
            auto t1 = heap.top(); heap.pop();
            auto t2 = heap.top(); heap.pop();
            int index1 = t1.second, index2 = t2.second;
            if (index1 >= 0) ids4.emplace_back(index1);
            if (index2 >= 0) ids4.emplace_back(index2);
            double s1 = t1.first;
            if (t1.second == -2) s1 = -s1;
            double s2 = t2.first;
            if (t2.second == -2) s2 = -s2;
            int index;
            if (s1 + s2 >= 0) index = -1;
            else index = -2;
            heap.push({fabs(s1 + s2), index});
        }

        // for (auto x: ids4) cout << x << ' '; cout << endl;

            string tmp = move(encode2(numbers, ids4));
            double ts = calculateScore(numbers, tmp);
            if (ts > mx_score) {
                mx_score = ts;
                ans = move(tmp);        
            }
    }

    cout << ans << endl;

    return 0;
}
