import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

dst = {}
dst[((0, 1), (-1, 1))] = 0
dst[((1, 1), (0, 1))] = 1
bfs_q = deque()
bfs_q.append(((1, 1), (0, 1), 1))
ans = 0
while len(bfs_q) > 0:
    cur_pt, prev_pt, cdst = bfs_q.popleft()
    if dst[(cur_pt, prev_pt)] != cdst:
        continue
    if cur_pt[0] == len(grid)-1:
        ans = max(ans, cdst)
        continue
    cx, cy = cur_pt
    nxts = [(-1, 0), (1, 0), (0, 1), (0, -1)]
    if grid[cx][cy] == "<": nxts = [(0, -1)]
    if grid[cx][cy] == ">": nxts = [(0, 1)]
    if grid[cx][cy] == "v": nxts = [(1, 0)]
    if grid[cx][cy] == "^": nxts = [(-1, 0)]
    for dx, dy in nxts:
        nx, ny = cx + dx, cy + dy
        if nx < 0: continue
        if ny < 0: continue
        if nx >= len(grid): continue
        if ny >= len(grid[nx]): continue
        if grid[nx][ny] == "#": continue
        if (nx, ny) == prev_pt: continue
        if grid[nx][ny] != ".":
            px, py = None, None
            if grid[nx][ny] == "<": px, py = (0, -1)
            if grid[nx][ny] == ">": px, py = (0, 1)
            if grid[nx][ny] == "v": px, py = (1, 0)
            if grid[nx][ny] == "^": px, py = (-1, 0)
            if (nx+px, ny+py) == cur_pt:
                continue
        if ((nx, ny), cur_pt) not in dst or dst[((nx, ny), cur_pt)] < dst[(cur_pt, prev_pt)] + 1:
            dst[((nx, ny), cur_pt)] = dst[(cur_pt, prev_pt)] + 1
            bfs_q.append(((nx, ny), cur_pt, dst[((nx, ny), cur_pt)]))

print(ans)
