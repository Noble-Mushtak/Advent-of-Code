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
        mxy = max(mxy, y)

OFFSET = 90000
grid = [[False for _ in range(mxy+10)] for _ in range(mxx+2*OFFSET)]

for x in range(mxx+2*OFFSET):
    grid[x][mxy+2] = True

for path in paths:
    for i in range(len(path)-1):
        pt1 = path[i]
        pt2 = path[i+1]
        if pt1 > pt2:
            pt1, pt2 = pt2, pt1
        if pt1[0] == pt2[0]:
            for y in range(pt1[1], pt2[1]+1):
                grid[pt1[0]+OFFSET][y] = True
        elif pt1[1] == pt2[1]:
            for x in range(pt1[0], pt2[0]+1):
                grid[x+OFFSET][pt1[1]] = True
        else:
            sys.exit(1)

ans = 0
while True:
    cx, cy = 500, 0
    if grid[cx+OFFSET][cy]:
        break
    while True:
        if cy > mxy+3:
            break
        if grid[cx+OFFSET][cy+1] == False:
            cy += 1
        elif grid[cx-1+OFFSET][cy+1] == False:
            cx -= 1
            cy += 1
        elif grid[cx+1+OFFSET][cy+1] == False:
            cx += 1
            cy += 1
        else:
            break
    if cy > mxy+3:
        break
    ans += 1
    grid[cx+OFFSET][cy] = True
    
print(ans)
