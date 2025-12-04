from collections import deque
import sys

grid = []
for line in sys.stdin:
    if line == "": continue
    grid.append(line)

def is_paper(i,j):
    if i < 0 or i >= len(grid): return False
    if j < 0 or j >= len(grid[i]): return False
    return grid[i][j] == "@"

neighbors = {}
bfs_q = deque()
added = set()
ds = [-1,0,1]

for i in range(len(grid)):
    for j in range(len(grid[i])):
        if not is_paper(i,j): continue
        subans = 0
        for dx in ds:
            for dy in ds:
                if (dx,dy)==(0,0): continue
                subans += int(is_paper(i+dx,j+dy))
        neighbors[(i,j)] = subans
        if subans < 4:
            bfs_q.append((i,j))
            added.add((i,j))

while len(bfs_q) > 0:
    x, y = bfs_q.popleft()
    assert is_paper(x,y)
    assert neighbors[(x,y)] < 4
    for dx in ds:
        for dy in ds:
            p = (x+dx,y+dy)
            if not is_paper(*p) or p in added: continue
            assert neighbors[p] >= 4
            neighbors[p] -= 1
            if neighbors[p] < 4:
                bfs_q.append(p)
                added.add(p)

print(len(added))
