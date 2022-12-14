import sys
import json
sys.setrecursionlimit(3000000)

mxx = -float("inf")
mxy = -float("inf")

paths = []
for line in sys.stdin:
    if line.strip() != "":
        toks = line.split(" -> ")
        paths.append([tuple(map(int, tok.split(","))) for tok in toks])


for path in paths:
    for x, y in path:
        mxx = max(mxx, x)
        mxy = max(mxx, y)

grid = [[False for _ in range(mxx+10)] for _ in range(mxy+10)]
for path in paths:
    for i in range(len(path)-1):
        pt1 = path[i]
        pt2 = path[i+1]
        if pt1 > pt2:
            pt1, pt2 = pt2, pt1
        if pt1[0] == pt2[0]:
            for y in range(pt1[1], pt2[1]+1):
                grid[pt1[0]][y] = True
        elif pt1[1] == pt2[1]:
            for x in range(pt1[0], pt2[0]+1):
                grid[x][pt1[1]] = True
        else:
            print("???")
            sys.exit(1)

ans = 0
while True:
    cx, cy = 500, 0
    while True:
        if cy > mxy:
            break
        if grid[cx][cy+1] == False:
            cy += 1
        elif grid[cx-1][cy+1] == False:
            cx -= 1
            cy += 1
        elif grid[cx+1][cy+1] == False:
            cx += 1
            cy += 1
        else:
            break
    if cy > mxy:
        break
    ans += 1
    grid[cx][cy] = True
    
print(ans)
