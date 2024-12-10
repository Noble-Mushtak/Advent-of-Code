import sys

loc_by_height = [[] for _ in range(10)]
reachable_locs = {}

grid = []
for line in sys.stdin:
    if line.strip() == "": continue
    grid.append(list(map(int, line.strip())))

N = len(grid)
M = len(grid[0])
for i in range(N):
    for j in range(M):
        loc_by_height[grid[i][j]].append((i,j))

for x, y in loc_by_height[9]:
    reachable_locs[(x,y)] = 1

ans = 0
    
dirs = [(1,0),(-1,0),(0,1),(0,-1)]
for h in range(8,-1,-1):
    for x, y in loc_by_height[h]:
        reachable_locs[(x,y)] = 0
        for dx, dy in dirs:
            newx,newy = x+dx,y+dy
            if (newx,newy) in reachable_locs and grid[newx][newy] == h+1:
                reachable_locs[(x,y)] += reachable_locs[(newx,newy)]
        if h == 0:
            print(x,y,reachable_locs[(x,y)])
            ans += reachable_locs[(x,y)]
print(ans)
