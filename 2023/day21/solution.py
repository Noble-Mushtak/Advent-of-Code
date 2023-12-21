import sys
from collections import defaultdict, deque

grid = []
st_loc = None
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    if "S" in line:
        st_loc = (len(grid), line.index("S"))
    grid.append(line)

STEPS = 64

locs = set()
locs.add(st_loc)
for i in range(STEPS):
    newlocs = set()
    for x, y in locs:
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx, ny = x+dx, y+dy
            if nx >= 0 and nx < len(grid) and ny >= 0 and ny < len(grid[0]) and grid[nx][ny] != "#":
                newlocs.add((nx, ny))
    locs = newlocs
print(len(locs))
