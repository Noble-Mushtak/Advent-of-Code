import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

start = None
start_adj = []
adj = defaultdict(list)
for i, row in enumerate(grid):
    for j, cell in enumerate(row):
        neighbors = []
        if cell == "|":
            neighbors = [(i-1, j), (i+1, j)]
        elif cell == "-":
            neighbors = [(i, j-1), (i, j+1)]
        elif cell == "L":
            neighbors = [(i-1, j), (i, j+1)]
        elif cell == "J":
            neighbors = [(i-1, j), (i, j-1)]
        elif cell == "7":
            neighbors = [(i+1, j), (i, j-1)]
        elif cell == "F":
            neighbors = [(i+1, j), (i, j+1)]
        elif cell == "S":
            start = (i, j)
        for x, y in neighbors:
            if x >= 0 and x < len(grid) and y >= 0 and y < len(row):
                adj[(i, j)].append((x, y))

adj_start = []
for vert in adj:
    for vert2 in adj[vert]:
        if vert2 == start:
            adj_start.append(vert)
adj[start] = adj_start

INF = 1000000000
dst = defaultdict(lambda: INF)
bfs_q = deque()
bfs_q.append(start)
dst[start] = 0
ans = (0, start)
while len(bfs_q) > 0:
    curcell = bfs_q.popleft()
    print(curcell, dst[curcell])
    for nxt in adj[curcell]:
        if dst[nxt] == INF:
            dst[nxt] = dst[curcell] + 1
            ans = max(ans, (dst[nxt], nxt))
            bfs_q.append(nxt)
print(ans[0])
