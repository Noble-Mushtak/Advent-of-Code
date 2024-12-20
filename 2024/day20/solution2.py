import sys
from collections import deque

ans = 0

grid = []

for line in sys.stdin:
    if line.strip() == "":
        continue
    grid.append(line.strip())

N = len(grid)
M = len(grid[0])

st = None
nd = None
for i in range(N):
    for j in range(M):
        if grid[i][j] == "S":
            st = (i,j)
        if grid[i][j] == "E":
            nd = (i,j)
    
dirs = [(0,1), (0,-1), (1,0), (-1,0)]

def solve():
    added = set()
    bfs_q = deque()

    prev = {}
    def add_node(x, y, dst, px, py):
        if (x,y) in added:
            return
        if px != None:
            prev[(x, y)] = (px,py)
        added.add((x, y))
        bfs_q.append((x, y, dst))

    add_node(st[0], st[1], 0, None, None)
    
    while len(bfs_q) > 0:
        cx, cy, dst = bfs_q[0]
        if cx == nd[0] and cy == nd[1]:
            path = [(cx, cy)]
            while path[-1] in prev:
                path.append(prev[path[-1]])
            return path
        
        bfs_q.popleft()
        for dx, dy in dirs:
            nx, ny = cx+dx, cy+dy
            if 0 <= nx < N and 0 <= ny < M and (grid[nx][ny] == "." or grid[nx][ny] == "E"):
                add_node(nx, ny, dst+1, cx, cy)

origans = solve()

for i in range(len(origans)):
    for j in range(i+1, len(origans)):
        dst = abs(origans[i][0] - origans[j][0]) + abs(origans[i][1] - origans[j][1])
        if dst > 20:
            continue
        new_path = i + dst + len(origans) - j - 1
        if new_path <= len(origans)-1-100:
            ans += 1

print(ans)
