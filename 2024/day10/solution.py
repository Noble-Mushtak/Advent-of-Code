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
    reachable_locs[(x,y)] = {(x,y)}

ans = 0
    
dirs = [(1,0),(-1,0),(0,1),(0,-1)]
for h in range(8,-1,-1):
    for x, y in loc_by_height[h]:
        reachable_locs[(x,y)] = set()
        for dx, dy in dirs:
            newx,newy = x+dx,y+dy
            if (newx,newy) in reachable_locs and grid[newx][newy] == h+1:
                for pt in reachable_locs[(newx,newy)]:
                    reachable_locs[(x,y)].add(pt)
        if h == 0:
            print(x,y,len(reachable_locs[(x,y)]))
            ans += len(reachable_locs[(x,y)])
print(ans)
