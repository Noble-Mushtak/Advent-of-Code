import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

rows_to_dup = []
for i, row in enumerate(grid):
    if "#" not in row:
        rows_to_dup.append(i)

new_grid = []
for i, row in enumerate(grid):
    new_grid.append(row)
    if i in rows_to_dup:
        new_grid.append(row)

cols_to_dup = []
for j in range(len(new_grid[0])):
    seen_hash = False
    for i in range(len(new_grid)):
        if new_grid[i][j] == "#":
            seen_hash = True
            break
    if not seen_hash:
        cols_to_dup.append(j)
        
new_grid2 = []
for i, row in enumerate(new_grid):
    new_row = []
    for j, cell in enumerate(row):
        new_row.append(cell)
        if j in cols_to_dup:
            new_row.append(cell)
    new_grid2.append("".join(new_row))

locs = []
for i, row in enumerate(new_grid2):
    for j, cell in enumerate(row):
        if cell == "#":
            locs.append((i, j))

ans = 0
for i, loc in enumerate(locs):
    for loc2 in locs[:i]:
        ans += abs(loc[0] - loc2[0]) + abs(loc[1] - loc2[1])
print(ans)
