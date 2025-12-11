from collections import defaultdict
import sys

adj = defaultdict(lambda: [])
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    name, toks = line.split(": ")
    adj[name] = toks.split()

def add_to_bm(bm, v):
    if v == "dac": return bm | 1
    if v == "fft": return bm | 2
    return bm

ans = defaultdict(lambda: 0)
dp = defaultdict(lambda: 0)
for src, dsts in adj.items():
    for dst in dsts:
        bm = add_to_bm(add_to_bm(0, src), dst)
        ans[(src, dst, bm)] = 1
        dp[(src, dst, bm)] = 1

while len(dp) > 0:
    new_dp = defaultdict(lambda: 0)
    for pr, v in dp.items():
        x, y, bm = pr
        for z in adj[y]:
            if x == z:
                continue
            new_dp[(x, z, add_to_bm(bm, z))] += v
    dp = new_dp
    for pr, v in dp.items():
        ans[pr] += v
print(ans[("svr", "out", 3)])
