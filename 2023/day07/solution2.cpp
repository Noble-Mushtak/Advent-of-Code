//replace Ofast with O3 for floating-point accuracy
#pragma GCC optimize("Ofast,unroll-loops")
#pragma GCC target("avx2,popcnt,lzcnt,abm,bmi,bmi2,fma")
#include <bits/stdc++.h>

using num = int64_t;
using namespace std;
#define rep(i, a, b) for(num i = a; i < (b); ++i)
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

num typeH(string S) {
    map<char, num> cnt;
    num numJs = 0;
    for (char c : S) {
        if (c == 'J') ++numJs;
        else ++cnt[c];
    }

    vector<pair<num, char>> cnts;
    for (auto [k,v] : cnt) cnts.push_back({v,k});
    sort(cnts.begin(), cnts.end());
    if (numJs == 5) return 6;
    if (cnts.back().first+numJs == 5) return 6;
    if (cnts.back().first+numJs == 4) return 5;
    if (sz(cnts) >= 2) {
        REPI(i, numJs+1) {
            if (cnts.back().first+i == 3 && cnts[sz(cnts)-2].first+(numJs-i) == 2) return 4;
        }
    }
    if (cnts.back().first+numJs == 3) return 3;
    if (sz(cnts) >= 2) {
        REPI(i, numJs+1) {
            if (cnts.back().first+i >= 2 && cnts[sz(cnts)-2].first+(numJs-i) >= 2) return 2;
        }
    }
    if (cnts.back().first+numJs == 2) return 1;
    return 0;
}

num cardval(char c) {
    if (c == 'T') return 10;
    if (c == 'J') return 0;
    if (c == 'Q') return 12;
    if (c == 'K') return 13;
    if (c == 'A') return 14;
    return c-'0';
}

bool compareHands(string S1, string S2) {
    num t1 = typeH(S1);
    num t2 = typeH(S2);
    if (t1 != t2) return t1 < t2;
    REPI(i, 5) {
        if (cardval(S1[i]) < cardval(S2[i])) {
            return true;
        }
        if (cardval(S1[i]) > cardval(S2[i])) {
            return false;
        }
    }
    return false;
}

int main() {
	cin.tie(0)->sync_with_stdio(0);
	cin.exceptions(cin.failbit);

    num N;
    cin >> N;
    vector<pair<string, num>> prs(N);
    REPI(i, N) {
        cin >> prs[i].first >> prs[i].second;
    }
    sort(prs.begin(), prs.end(), [&](pair<string, num> p1, pair<string, num> p2) { return compareHands(p1.first, p2.first); });
    num ans = 0;
    REPI(i, N) {
        cout << prs[i].second << "\n";
        ans += prs[i].second * (i+1);
    }
    cout << ans << "\n";
}
