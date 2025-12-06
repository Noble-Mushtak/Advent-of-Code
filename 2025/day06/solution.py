import sys

grid = []
for line in sys.stdin:
    line=line.strip()
    if line=="": continue
    grid.append(list(line.split()))
dimen=0
for i, row in enumerate(grid):
    if i==0:
        dimen = len(row)
    else:
        assert dimen == len(row)
    if i<len(grid)-1:
        for j in range(dimen):
            grid[i][j]=int(grid[i][j])
realans=0
for j in range(dimen):
    mult = grid[-1][j] == "*"
    ans = int(mult)
    for i in range(len(grid)-1):
        if mult: ans *= grid[i][j]
        else: ans += grid[i][j]
    realans+=ans
print(realans)
