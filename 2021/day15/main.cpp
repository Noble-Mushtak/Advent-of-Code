//change to O3 to disable fast-math for floating-point/geometry problems
#pragma GCC optimize("Ofast,unroll-loops")
//avx2 alternatives for older platforms: sse/sse2/sse3/sse4/sse4.1/sse4.2/avx
#pragma GCC target("avx2,popcnt,lzcnt,abm,bmi,bmi2,fma")
//#pragma GCC target("avx2,popcnt,lzcnt,abm,bmi,bmi2,fma,tune=native")
#include <bits/stdc++.h>

#define REP(token, num) for (token = 0; token < num; ++token)
#define REPI(token, num) for (int64_t token = 0; token < num; ++token)
#define REPIT(type, token, num) for (type token = 0; token < num; ++token)

#ifdef TESTING
#define DEBUG(...) __VA_ARGS__
#else
#define DEBUG(...)
#endif

typedef int64_t num;
using namespace std;

const num INF = 1e18;
const vector<pair<num, num>> ns = {{0,1},{0,-1},{1,0},{-1,0}};
template<class D> num dijkstra(num numVs, D getCost) {
    auto checkValid = [&](num a) { return (a >= 0) && (a < numVs); };
    pair end = {numVs-1, numVs-1};
    vector<vector<num>> dists(numVs, vector<num>(numVs, INF));
    
    priority_queue<pair<num, pair<num, num>>> dijkQ;
    dijkQ.push({0, {0,0}});
    dists[0][0] = 0;
    
    while (!dijkQ.empty()) {
        num cost;
        pair<num, num> loc;
        tie(cost, loc) = dijkQ.top();
        cost *= -1;
        dijkQ.pop();
        if (cost > dists[loc.first][loc.second]) continue;
        if (loc == end) return cost;
        
        for (auto n : ns) {
            if (checkValid(loc.first+n.first) && checkValid(loc.second+n.second)) {
                if (dists[loc.first+n.first][loc.second+n.second] > cost+getCost(loc.first+n.first, loc.second+n.second)) {
                    dists[loc.first+n.first][loc.second+n.second] = cost+getCost(loc.first+n.first, loc.second+n.second);
                    dijkQ.push({-dists[loc.first+n.first][loc.second+n.second], {loc.first+n.first, loc.second+n.second}});
                }
            }
        }
    }
    cerr << "ANSWER NOT FOUND\n";
    exit(1);
}

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    num N;
    vector<vector<num>> maze;
    REPI(i, N) {
        string s;
        cin >> s;
        if (i == 0) {
            N = s.size();
            maze = vector<vector<num>>(N, vector<num>(N));
        }
        REPI(j, N) maze[i][j] = (s[j]-'0');
    }
    auto getMaze = [&](num a, num b) { return maze[a][b]; };
    cout << "Part 1: " << dijkstra(N, getMaze) << "\n";
    auto getMaze2 = [&](num a, num b) {
                        num x = a/N;
                        num y = b/N;
                        num cur = maze[a % N][b % N];
                        num c = (cur+x+y) % 9;
                        if (c == 0) return (num)9;
                        return c;
                    };
    cout << "Part 2: " << dijkstra(5*N, getMaze2) << "\n";

    return 0;
}
