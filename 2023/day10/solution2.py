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
            neighbors = [(2*i-1, 2*j), (2*i+1, 2*j)]
        elif cell == "-":
            neighbors = [(2*i, 2*j-1), (2*i, 2*j+1)]
        elif cell == "L":
            neighbors = [(2*i-1, 2*j), (2*i, 2*j+1)]
        elif cell == "J":
            neighbors = [(2*i-1, 2*j), (2*i, 2*j-1)]
        elif cell == "7":
            neighbors = [(2*i+1, 2*j), (2*i, 2*j-1)]
        elif cell == "F":
            neighbors = [(2*i+1, 2*j), (2*i, 2*j+1)]
        elif cell == "S":
            start = (2*i, 2*j)
        for x, y in neighbors:
            if x >= 0 and x < 2*len(grid) and y >= 0 and y < 2*len(row):
                adj[(2*i, 2*j)].append((x, y))

for i, row in enumerate(grid):
    for j, cell in enumerate(row):
        xs = []
        if i > 0: xs.append(2*i-1)
        if i+1 < len(grid): xs.append(2*i+1)
        ys = []
        if j > 0: ys.append(2*j-1)
        if j+1 < len(row): ys.append(2*j+1)
        for nx in xs:
            adj[(nx, 2*j)].append((2*i, 2*j))
        for ny in ys:
            adj[(2*i, ny)].append((2*i, 2*j))
                
inv_start = []
indeg = defaultdict(int)
for vert in adj:
    for vert2 in adj[vert]:
        indeg[vert2] += 1
        if vert2 == start:
            inv_start.append(vert)
for vert in inv_start:
    if indeg[vert] > 0:
        adj[start].append(vert)

INF = 1000000000
dst = defaultdict(lambda: INF)
bfs_q = deque()
bfs_q.append(start)
dst[start] = 0
ans = (0, start)
inloop = set()
while len(bfs_q) > 0:
    curcell = bfs_q.popleft()
    inloop.add(curcell)
    # print(curcell, dst[curcell])
    for nxt in adj[curcell]:
        if dst[nxt] == INF:
            dst[nxt] = dst[curcell] + 1
            ans = max(ans, (dst[nxt], nxt))
            bfs_q.append(nxt)
# print(ans[0])
# print(inloop)

ans2 = 0
vis = set()
for i, row in enumerate(grid):
    for j, cell in enumerate(row):
        if (2*i, 2*j) in inloop or (2*i, 2*j) in vis:
            continue
        added_to_q = set()
        cur_q = deque()
        cur_q.append((2*i, 2*j))
        added_to_q.add((2*i, 2*j))
        enclosed = True
        while len(cur_q) > 0:
            cx, cy = cur_q.popleft()
            # print(i, j, cx, cy)
            for nx, ny in [(cx-1, cy), (cx+1, cy), (cx, cy-1), (cx, cy+1)]:
                # print(i, j, cx, cy, nx, ny, (nx, ny) in inloop, (nx, ny) in added_to_q)
                if (nx, ny) in inloop or (nx, ny) in added_to_q:
                    continue
                assert((nx, ny) not in vis)
                if nx < 0 or nx >= 2*len(grid) or ny < 0 or ny >= 2*len(row):
                    enclosed = False
                    continue
                cur_q.append((nx, ny))
                added_to_q.add((nx, ny))
        for c in added_to_q:
            if c[0] % 2 == 0 and c[1] % 2 == 0 and enclosed:
                print("HEY", c)
                ans2 += 1
            vis.add(c)
print(ans2)
