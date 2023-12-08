import sys
from collections import defaultdict

first = True
dirs = ""
mps = {}
for line in sys.stdin:
    if line.strip() == "": continue
    if first:
        first = False
        dirs = line.strip()
    else:
        a, b, c, d = line.strip().split()
        mps[a] = (c[1:-1], d[:-1])

st = "AAA"
ans = 0
idx = 0
while st != "ZZZ":
    if dirs[idx % len(dirs)] == "L":
        st = mps[st][0]
    else:
        st = mps[st][1]
    ans += 1
    idx += 1
print(ans)
