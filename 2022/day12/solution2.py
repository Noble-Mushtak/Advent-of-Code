import sys
from collections import deque

grid = []
for line in sys.stdin:
    if line.strip() != "":
        grid.append(list(line.strip()))

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
        
cur_loc = None
end_loc = None
for i, line in enumerate(grid):
    for j, ch in enumerate(line):
        if ch == "S":
            cur_loc = (i, j)
        elif ch == "E":
            end_loc = (i, j)
grid[cur_loc[0]][cur_loc[1]] = "a"
grid[end_loc[0]][end_loc[1]] = "z"

a_locs = []
for i, line in enumerate(grid):
    for j, ch in enumerate(line):
        if ch == "a":
            a_locs.append((i, j))

inq = {}
bfsq = deque()
for x, y in a_locs:
    bfsq.append((x, y))
    inq[(x, y)] = 0
    
while len(bfsq) > 0:
    cur_x, cur_y = bfsq.popleft()
    dst = inq[(cur_x, cur_y)]
    print(cur_x, cur_y, dst)
    if (cur_x, cur_y) == end_loc:
        print(dst)
        break
    for dx, dy in dirs:
        new_x =cur_x+dx
        new_y=cur_y+dy
        if new_x < 0: continue
        if new_y < 0: continue
        try:
            cur_val = grid[new_x][new_y]
            if ord(cur_val) <= ord(grid[cur_x][cur_y])+1:
                if (new_x, new_y) not in inq:
                    inq[(new_x, new_y)] = dst+1
                    bfsq.append((new_x, new_y))
        except IndexError:
            continue
