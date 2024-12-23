import copy
import sys
from collections import deque

ans = 0
connected = set()
verts = set()

for line in sys.stdin:
    if line.strip() == "":
        continue
    a, b = tuple(line.strip().split("-"))
    connected.add((a, b))
    connected.add((b, a))
    verts.add(a)
    verts.add(b)
verts = list(verts)
for i, a in enumerate(verts):
    for j, b in enumerate(verts[:i]):
        for c in verts[:j]:
            if (a.startswith("t") or b.startswith("t") or c.startswith("t")) and ((a,b) in connected) and ((b,c) in connected) and ((c,a) in connected):
                ans += 1
    
print(ans)
