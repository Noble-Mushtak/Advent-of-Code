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

def solve(sx, sy, ex, ey):
    added = set()
    bfs_q = deque()

    def add_node(x, y, used, dst):
        if (x,y,used) in added:
            return
        added.add((x, y, used))
        bfs_q.append((x, y, used, dst))

    add_node(st[0], st[1], False, 0)
    
    while len(bfs_q) > 0:
        cx, cy, used, dst = bfs_q[0]
        #print(cx, cy, used, dst)
        if cx == nd[0] and cy == nd[1]:
            return dst
        
        bfs_q.popleft()
        if cx == sx and cy == sy:
            add_node(ex, ey, used, dst+1)
        else:
            for dx, dy in dirs:
                nx, ny = cx+dx, cy+dy
                if 0 <= nx < N and 0 <= ny < M and (grid[nx][ny] == "." or grid[nx][ny] == "E" or (nx == sx and ny == sy and not used)):
                    new_used = (nx == sx and ny == sy and not used)
                    add_node(nx, ny, new_used, dst+1)

origans = solve(-1,-1,-1,-1)

for i in range(N):
    for j in range(M):
        if grid[i][j] == "#":
            for dx, dy in dirs:
                nx, ny = i+dx, j+dy
                if 0 <= nx < N and 0 <= ny < M and (grid[nx][ny] == "." or grid[nx][ny] == "E"):
                    newans = solve(i,j,nx,ny)
                    if newans <= origans-100:
                        print(i,j,nx,ny, newans, origans)
                        ans += 1

print(ans)
