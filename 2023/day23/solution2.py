import sys
from collections import defaultdict, deque
sys.setrecursionlimit(3000000)

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

ans = 0
vis = set()
def dfs(cur_pos):
    global ans
    if cur_pos in vis:
        return
    if cur_pos[0] == len(grid)-1:
        if len(vis) > ans:
            print(len(vis))
        ans = max(ans, len(vis))
        return

    vis.add(cur_pos)
    
    cx, cy = cur_pos
    for dx, dy in [(-1, 0), (1, 0), (0, 1), (0, -1)]:
        nx, ny = cx + dx, cy + dy
        if nx < 0: continue
        if ny < 0: continue
        if nx >= len(grid): continue
        if ny >= len(grid[nx]): continue
        if grid[nx][ny] == "#": continue
        if (nx, ny) in vis: continue
        dfs((nx, ny))

    vis.remove(cur_pos)

dfs((0, 1))
print(ans)
