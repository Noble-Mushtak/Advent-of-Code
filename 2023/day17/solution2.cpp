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

struct st {
    num x, y, dx, dy, numLeft;
};
bool operator<(st st1, st st2) {
    if (st1.x != st2.x) return st1.x < st2.x;
    if (st1.y != st2.y) return st1.y < st2.y;
    if (st1.dx != st2.dx) return st1.dx < st2.dx;
    if (st1.dy != st2.dy) return st1.dy < st2.dy;
    return st1.numLeft < st2.numLeft;
}

int main() {
	cin.tie(0)->sync_with_stdio(0);
	cin.exceptions(cin.failbit);

    num N;
    cin >> N;
    vector<string> grid(N);
    REPI(i, N) cin >> grid[i];

    set<st> processed;
    priority_queue<pair<num, st>> dijkQ;
    st initSt = {0, 0, 1, 0, 10};
    dijkQ.push({0, initSt});
    while (!dijkQ.empty()) {
        num c;
        st curSt;
        tie(c, curSt) = dijkQ.top();
        c *= -1;
        dijkQ.pop();
        //cout << c << " " << curSt.x << " " << curSt.y << " " << curSt.dx << " " << curSt.dy << " " << curSt.numLeft << "\n";
        if ((curSt.x == sz(grid)-1) && (curSt.y == sz(grid.back())-1) && (curSt.numLeft <= 6)) {
            cout << curSt.dx << " " << curSt.dy << " " << curSt.numLeft << "\n";
            cout << c << "\n";
            break;
        }
        if (processed.count(curSt)) continue;
        processed.insert(curSt);

        vector<pair<num,num>> dirs = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};
        for (auto [newdx, newdy] : dirs) {
            if ((newdx == -curSt.dx) && (newdy == -curSt.dy)) continue;
            num newLeft = 9;
            if ((newdx == curSt.dx) && (newdy == curSt.dy)) {
                if (curSt.numLeft == 0) continue;
                newLeft = curSt.numLeft - 1;
            } else {
                if (curSt.numLeft > 6) continue;
            }
            num newx = curSt.x + newdx, newy = curSt.y + newdy;
            if (newx < 0) continue;
            if (newy < 0) continue;
            if (newx >= sz(grid)) continue;
            if (newy >= sz(grid[newx])) continue;
            st newSt = {newx, newy, newdx, newdy, newLeft};
            dijkQ.push({ -(c + grid[newx][newy] - '0'), newSt });
        }
    }
    cout << "NOT FOUND\n";
}
