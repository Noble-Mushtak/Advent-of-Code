import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

def rotate_grid(grid):
    ngrid = [["." for _ in range(len(grid))] for _ in range(len(grid[0]))]
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            ngrid[j][i] = grid[i][j]
    for i in range(len(grid[0])):
        ngrid[i] = list(reversed(ngrid[i]))
    return ngrid
    
def transform_grid(grid, rots=0):
    rots %= 4
    for i in range(rots):
        grid = rotate_grid(grid)
        
    ngrid = [["." for _ in range(len(grid[0]))] for _ in range(len(grid))]
    ans = 0
    for j in range(len(grid[0])):
        os_in_queue = 0
        for i in range(len(grid)-1, -1, -1):
            if grid[i][j] == "#":
                for k in range(os_in_queue):
                    ans += len(grid)-(i+1+k)
                    ngrid[i+1+k][j] = "O"
                os_in_queue = 0
                ngrid[i][j] = "#"
            elif grid[i][j] == "O":
                os_in_queue += 1
        for k in range(os_in_queue):
            ans += len(grid)-k
            ngrid[k][j] = "O"
            
    for i in range(4-rots):
        ngrid = rotate_grid(ngrid)
        
    return ans, ngrid

def cycle_grid(grid):
    for i in range(4):
        grid = transform_grid(grid, i)[1]
    return grid

def answer_grid(grid):
    ans = 0
    for i in range(len(grid)):
        for ch in grid[i]:
            if ch == "O":
                ans += len(grid)-i
    return ans

sofar = {}
steps = {}
steps[0] = grid
sofar[str(grid)] = 0
X = 1000000000
ans = None
for i in range(X):
    grid = cycle_grid(grid)
    print(i+1, transform_grid(grid)[0], answer_grid(grid))
    if str(grid) in sofar:
        ans = i+1, sofar[str(grid)]
        break
    steps[i+1] = grid
    sofar[str(grid)] = i+1

print(answer_grid(steps[((X - ans[1]) % (ans[0] - ans[1])) + ans[1]]))
