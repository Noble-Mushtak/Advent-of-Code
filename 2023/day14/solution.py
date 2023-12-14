import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)
        
ans = 0
for j in range(len(grid[0])):
    os_in_queue = 0
    for i in range(len(grid)-1, -1, -1):
        if grid[i][j] == "#":
            for k in range(os_in_queue):
                ans += len(grid)-(i+1+k)
            os_in_queue = 0
        elif grid[i][j] == "O":
            os_in_queue += 1
    for k in range(os_in_queue):
        ans += len(grid)-k
print(ans)
