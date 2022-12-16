//replace Ofast with O3 for floating-point accuracy
#pragma GCC optimize("Ofast,unroll-loops")
#pragma GCC target("avx2,popcnt,lzcnt,abm,bmi,bmi2,fma")
#include <bits/stdc++.h>

using num = int64_t;
using namespace std;
#define rep(i, a, b) for(int i = a; i < (b); ++i)
#define REPI(t, n) for (num t = 0; t < n; ++t)
#define all(x) begin(x), end(x)
#define sz(x) (int)(x).size()
using ll = long long;
using pii = pair<int, int>;
using vi = vector<int>;
#ifdef TESTING
#define DEBUG(...) __VA_ARGS__
#else
#define DEBUG(...)
#endif

struct state {
    string curV;
    num bm;
    num timeLeft;
    num curRate;
};

bool operator<(state st1, state st2) {
    pair<string,pair<num,num>> pr1 = {st1.curV,{st1.bm,st1.timeLeft}};
    pair<string,pair<num,num>> pr2 = {st2.curV,{st2.bm,st2.timeLeft}};
    return pr1 < pr2;
}

struct dp {
    map<string, vector<string>> edges;
    map<string, pair<num,num>> rates;
    map<state, num> res;

    num calcRes(state st) {
        if (st.timeLeft == 0) return 0;
        auto it = res.find(st);
        if (it != res.end()) return it->second;
        auto it2 = rates.find(st.curV);
        num ans = 0;
        if (it2 != rates.end()) {
            num bmIdx, newRate;
            tie(newRate,bmIdx) = it2->second;
            if (((st.bm >> bmIdx) & 1) == 0) {
                state newSt = {st.curV, st.bm | (1 << bmIdx), st.timeLeft-1, st.curRate+newRate};
                ans = max(ans, st.curRate+newRate+calcRes(newSt));
            }
        }
        for (string newV : edges[st.curV]) {
            state newSt = {newV, st.bm, st.timeLeft-1, st.curRate};
            ans = max(ans, st.curRate+calcRes(newSt));
        }
        res[st] = ans;
        return ans;
    }
};

int main() {
	cin.tie(0)->sync_with_stdio(0);
	cin.exceptions(cin.failbit);

    num N;
    cin >> N;
    map<string, pair<num,num>> rates;
    map<string, vector<string>> edges;
    REPI(i, N) {
        string name;
        num curRate, numEdges;
        cin >> name >> curRate >> numEdges;
        if (curRate != 0) {
            num idx = sz(rates);
            rates[name] = {curRate,idx};
        }
        REPI(j, numEdges) {
            string name2;
            cin >> name2;
            edges[name].push_back(name2);
        }
    }
    dp curDp;
    curDp.edges = edges;
    
    map<num, num> answers;
    REPI(bm, 1 << sz(rates)) {
        if (abs(__builtin_popcountll((ll)bm)-sz(rates)/2) <= 1) {
            curDp.rates.clear();
            curDp.res.clear();
            for (auto [k, pr] : rates) {
                if ((bm >> pr.second) & 1) {
                    curDp.rates.insert({k, pr});
                }
            }
            num curAns = curDp.calcRes({"AA", 0, 29-4, 0});
            answers[bm] = curAns;
            cout << sz(answers) << "\n";
            cout.flush();
        }
    }
    num maxBm = (1 << sz(rates))-1;
    num ans = 0;
    for (auto [k, v] : answers) {
        auto it = answers.find(maxBm ^ k);
        if (it != answers.end()) {
            num otherV = it->second;
            ans = max(ans, v+otherV);
        }
    }
    cout << ans << "\n";
}
