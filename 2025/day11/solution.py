from collections import defaultdict
import sys

adj = defaultdict(lambda: [])
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    name, toks = line.split(": ")
    adj[name] = toks.split()

ans = defaultdict(lambda: 0)
dp = defaultdict(lambda: 0)
for src, dsts in adj.items():
    for dst in dsts:
        ans[(src, dst)] = 1
        dp[(src, dst)] = 1

while len(dp) > 0:
    new_dp = defaultdict(lambda: 0)
    for pr, v in dp.items():
        x, y = pr
        for z in adj[y]:
            if x == z:
                continue
            new_dp[(x, z)] += dp[(x, y)]
    dp = new_dp
    for pr, v in dp.items():
        ans[pr] += v
print(ans[("you", "out")])
