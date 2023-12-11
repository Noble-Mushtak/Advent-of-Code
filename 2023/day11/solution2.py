import sys
from collections import defaultdict, deque

FACTOR = 1000000

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

xps = [0]
for i, row in enumerate(grid):
    if "#" not in row:
        xps.append(xps[-1] + FACTOR)
    else:
        xps.append(xps[-1] + 1)

yps = [0]
for j in range(len(grid[0])):
    seen_hash = False
    for i in range(len(grid)):
        if grid[i][j] == "#":
            seen_hash = True
            break
    if not seen_hash:
        yps.append(yps[-1] + FACTOR)
    else:
        yps.append(yps[-1] + 1)

locs = []
for i, row in enumerate(grid):
    for j, cell in enumerate(row):
        if cell == "#":
            locs.append((i, j))

ans = 0
for i, loc in enumerate(locs):
    for loc2 in locs[:i]:
        ans += abs(xps[loc[0]] - xps[loc2[0]]) + abs(yps[loc[1]] - yps[loc2[1]])
print(ans)


        
