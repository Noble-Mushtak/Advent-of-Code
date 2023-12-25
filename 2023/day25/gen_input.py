import sys
from collections import defaultdict, deque
from fractions import Fraction

adj = defaultdict(list)
ans = 0
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    a, b = line.split(": ")
    for x in b.split():
        adj[a].append(x)
        adj[x].append(a)
        ans += 1
    
names = {}
name_arr = []
n = 0
for k in adj:
    names[k] = n
    name_arr.append(k)
    n += 1

print(n)
for i in range(n):
    print(len(adj[name_arr[i]]))
    for v in adj[name_arr[i]]:
        print(names[v], end=" ")
    print()
