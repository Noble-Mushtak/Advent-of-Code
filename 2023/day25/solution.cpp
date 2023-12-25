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

pair<int, vi> globalMinCut(vector<vi> mat) {
	pair<int, vi> best = {INT_MAX, {}};
	int n = sz(mat);
    
    cout << n << "\n";
    cout.flush();
    
	vector<vi> co(n);
	rep(i,0,n) co[i] = {i};
	rep(ph,1,n) {
		vi w = mat[0];
		size_t s = 0, t = 0;
		rep(it,0,n-ph) { // O(V^2) -> O(E log V) with prio. queue
			w[t] = INT_MIN;
			s = t, t = max_element(all(w)) - w.begin();
			rep(i,0,n) w[i] += mat[t][i];
		}
		best = min(best, {w[t] - mat[t][t], co[t]});
		co[s].insert(co[s].end(), all(co[t]));
		rep(i,0,n) mat[s][i] += mat[t][i];
		rep(i,0,n) mat[i][s] = mat[s][i];
		mat[0][t] = INT_MIN;
	}
	return best;
}

int main() {
	cin.tie(0)->sync_with_stdio(0);
	cin.exceptions(cin.failbit);

    num N;
    cin >> N;
    vector<vector<int>> adj(N, vector<int>(N, 0));
    REPI(i, N) {
        num L;
        cin >> L;
        REPI(j, L) {
            num x;
            cin >> x;
            adj[i][x] = adj[x][i] = 1;
        }
    }
    auto ans = globalMinCut(adj);
    cout << ans.first << "\n";
    cout << sz(ans.second) << " " << N-sz(ans.second) << "\n";
    cout << (sz(ans.second) * (N-sz(ans.second))) << "\n";
}
