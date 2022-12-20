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

int main() {
	cin.tie(0)->sync_with_stdio(0);
	cin.exceptions(cin.failbit);

    num N;
    cin >> N;
    deque<pair<num,num>> a(N);
    REPI(i, N) {
        cin >> a[i].first;
        a[i].second = i;
    }
    REPI(i, N) {
        num idx = -1;
        REPI(j, N) {
            if (a[j].second == i) {
                idx = j;
                break;
            }
        }
        assert(idx != -1);
        num val = a[idx].first;
        if (val < 0) {
            num times = -val;
            REPI(k, times) {
                if (idx == 0) {
                    a.pop_front();
                    a.push_back({val, i});
                    idx = N-1;
                }
                    swap(a[idx-1], a[idx]);
                    --idx;
                if (idx == 0) {
                    a.pop_front();
                    a.push_back({val, i});
                    idx = N-1;
                }
            }
        } else {
            REPI(k, val) {
                if (idx == N-1) {
                    a.pop_back();
                    a.push_front({val, i});
                    idx = 0;
                }
                    swap(a[idx], a[idx+1]);
                    ++idx;
                if (idx == N-1) {
                    a.pop_back();
                    a.push_front({val, i});
                    idx = 0;
                }
            }
        }   
    }
    num zeroIdx = -1;
    REPI(i, N) {
        if (a[i].first == 0) {
            zeroIdx = i;
            break;
        }
    }
    assert(zeroIdx != -1);
    cout << (a[(1000+zeroIdx) % N].first+a[(2000+zeroIdx) % N].first+a[(3000+zeroIdx) % N].first) << "\n";
}
