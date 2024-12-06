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

def solve(specx, specy):
    visited = set()
    dir_index = 0
    cur_loc = loc
    while cur_loc[0] >= 0 and cur_loc[0] < N and cur_loc[1] >= 0 and cur_loc[1] < M:
        pdir_index = dir_index
        curx, cury = cur_loc
        next_loc = (curx+dirs[dir_index][0], cury+dirs[dir_index][1])
        while next_loc[0] >= 0 and next_loc[0] < N and next_loc[1] >= 0 and next_loc[1] < M and (grid[next_loc[0]][next_loc[1]] == "#" or next_loc == (specx, specy)):
            dir_index = (dir_index+1) % 4
            next_loc = (curx+dirs[dir_index][0], cury+dirs[dir_index][1])
        visited.add((cur_loc, pdir_index))
        cur_loc = next_loc
        if (next_loc, dir_index) in visited:
            return True
    return False

for i in range(N):
    for j in range(M):
        if grid[i][j] == ".":
            b = solve(i, j)
            ans += int(b)
            if b:
                print(i, j)

print(ans)
