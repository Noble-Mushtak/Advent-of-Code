import sys
from collections import defaultdict
l = []
r = []
for line in sys.stdin:
    if line.strip() == "": continue
    a, b = map(int, line.split())
    l.append(a)
    r.append(b)

rd = defaultdict(lambda: 0)
for x in r:
    rd[x] += 1
ans = 0
for el in l:
    ans += el * rd[el]
print(ans)
