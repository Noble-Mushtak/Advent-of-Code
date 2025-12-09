import sys

points = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    points.append(tuple(map(int, line.split(","))))

xset = set()
yset = set()
for x, y in points:
    xset.add(x)
    xset.add(x+1)
    yset.add(y)
    yset.add(y+1)
xlst = list(xset)
ylst = list(yset)
xlst.sort()
ylst.sort()
xmap = {}
ymap = {}
for i, x in enumerate(xlst):
    xmap[x] = i
for i, y in enumerate(ylst):
    ymap[y] = i

grid = [[0 for _ in ylst] for _ in xlst]
for i, pt in enumerate(points):
    x1, y1 = pt
    x2, y2 = points[(i+1) % len(points)]
    x1, x2 = xmap[x1], xmap[x2]
    y1, y2 = ymap[y1], ymap[y2]
    if x1 != x2:
        assert y1 == y2
        if x1 > x2:
            x1, x2 = x2, x1
        grid[x1][y1] |= 1
        grid[x2][y1] |= 2
        for x in range(x1+1, x2):
            grid[x][y1] |= 3

realgrid = [[False for _ in ylst] for _ in xlst]
for i, row in enumerate(grid):
    nxt = 0
    for j, cell in enumerate(row):
        realgrid[i][j] = nxt > 0 or cell > 0
        nxt ^= cell
    assert nxt == 0, (j, row)
    """
    for cell in realgrid[i]:
        print("#" if cell else ".", end="")
    print()
    """

realgridsums = [[0 for _ in range(len(ylst)+1)] for _ in range(len(xlst)+1)]
for i, row in enumerate(realgrid):
    for j, cell in enumerate(row):
        realgridsums[i+1][j+1] = int(cell) + realgridsums[i][j+1] + realgridsums[i+1][j] - realgridsums[i][j]

ans = 0
for i, pt in enumerate(points):
    for j, pt2 in enumerate(points[:i]):
        x1,y1=pt
        x2,y2=pt2
        if x1 > x2:
            x1,x2 = x2,x1
        if y1 > y2:
            y1,y2 = y2,y1
        xr1,xr2=xmap[x1],xmap[x2]
        yr1,yr2=ymap[y1],ymap[y2]
        num_goods = realgridsums[xr2+1][yr2+1]-realgridsums[xr2+1][yr1]-realgridsums[xr1][yr2+1]+realgridsums[xr1][yr1]
        if num_goods == (xr2-xr1+1)*(yr2-yr1+1):
            ans = max(ans, (x2-x1+1)*(y2-y1+1))
print(ans)
