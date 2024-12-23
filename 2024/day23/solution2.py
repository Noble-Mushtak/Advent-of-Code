import copy
import sys
from collections import deque
import networkx

G = networkx.Graph()

ans = 0
connected = set()
verts = set()

for line in sys.stdin:
    if line.strip() == "":
        continue
    a, b = tuple(line.strip().split("-"))
    connected.add((a, b))
    verts.add(a)
    verts.add(b)

for v in verts:
    G.add_node(v)

for a, b in connected:
    G.add_edge(a, b)

print(G.nodes)
best = []
for cliq in networkx.find_cliques(G):
    if len(cliq) > len(best):
        best = cliq
best.sort()
print(",".join(best))
