import sys
from collections import defaultdict, deque

ans = 0
grid = []

def process_grid1(grid):
    hr = None
    for i in range(1, len(grid)):
        good = True
        for j in range(i):
            refl_row = 2*i-1-j
            if refl_row >= 0 and refl_row < len(grid) and grid[j] != grid[refl_row]:
                # print("BAD", i, j, refl_row, grid[j], grid[refl_row])
                good = False
                break
        if good:
            hr = i
            break
    return hr

def process_grid(grid):
    hr = process_grid1(grid)
    if hr == None:
        hr = 0
    
    cols = ["" for _ in range(len(grid[0]))]
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            cols[j] += grid[i][j]
            
    ans = process_grid1(cols)
    if ans == None:
        ans = 0

    if hr == 0 and ans == 0:
        for line in grid: print(line)
        print()
        
    return 100*hr + ans

for line in sys.stdin:
    line = line.strip()
    if line == "":
        ans += process_grid(grid)
        grid = []
        continue
    grid.append(line)

if len(grid) > 0:
    ans += process_grid(grid)
    
print(ans)
