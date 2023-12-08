import math
import sys
from collections import defaultdict

first = True
dirs = ""
mps = {}
verts = []
for line in sys.stdin:
    if line.strip() == "": continue
    if first:
        first = False
        dirs = line.strip()
    else:
        a, b, c, d = line.strip().split()
        mps[a] = (c[1:-1], d[:-1])
        verts.append(a)

def solve_vert(x):
    answers = []
    idx = 0
    prev = {}
    ans = 0
    while (x, idx % len(dirs)) not in prev:
        prev[(x, idx % len(dirs))] = ans
        if dirs[idx % len(dirs)] == "L":
            x = mps[x][0]
        else:
            x = mps[x][1]
        ans += 1
        idx += 1
        if x[-1] == "Z":
            answers.append((ans, x))
    return answers, prev[(x, idx % len(dirs))], ans

ans = 1
curs = []
for vert in verts:
    if vert[-1] == "A":
        curs.append(vert)
        n = solve_vert(vert)[0][0][0]
        ans = ans*n//math.gcd(ans, n)

print(ans)
