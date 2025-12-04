import sys

grid = []
for line in sys.stdin:
    if line == "": continue
    grid.append(line)

def is_paper(i,j):
    if i < 0 or i >= len(grid): return False
    if j < 0 or j >= len(grid[i]): return False
    return grid[i][j] == "@"

ans = 0
for i in range(len(grid)):
    for j in range(len(grid[i])):
        if not is_paper(i,j): continue
        ds = [-1,0,1]
        subans = 0
        for dx in ds:
            for dy in ds:
                if (dx,dy)==(0,0): continue
                subans += int(is_paper(i+dx,j+dy))
        ans += int(subans < 4)
print(ans)
