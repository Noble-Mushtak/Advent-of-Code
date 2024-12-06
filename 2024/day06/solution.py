import sys

ans = 0

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

grid = []
for line in sys.stdin:
    if line.strip() == "":
        continue
    grid.append(line.strip())

N = len(grid)
M = len(grid[0])
loc = None
for i in range(N):
    for j in range(M):
        if grid[i][j] == "^":
            loc = (i, j)
            break

visited = set()
dir_index = 0
cur_loc = loc
while cur_loc[0] >= 0 and cur_loc[0] < N and cur_loc[1] >= 0 and cur_loc[1] < M:
    curx, cury = cur_loc
    next_loc = (curx+dirs[dir_index][0], cury+dirs[dir_index][1])
    while next_loc[0] >= 0 and next_loc[0] < N and next_loc[1] >= 0 and next_loc[1] < M and grid[next_loc[0]][next_loc[1]] == "#":
        dir_index = (dir_index+1) % 4
        next_loc = (curx+dirs[dir_index][0], cury+dirs[dir_index][1])
    visited.add(cur_loc)
    cur_loc = next_loc

print(len(visited))
