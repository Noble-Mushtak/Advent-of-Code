import sys

grid = []
for line in sys.stdin:
    if line=="": continue
    grid.append(line)
dimen=0
for i, row in enumerate(grid):
    dimen = max(dimen,len(row))
op = None
nums = []
realans = 0
def flush():
    global realans
    global nums
    if op is None:
        return
    ans = op
    for n in nums:
        if op == 1: ans *= n
        else: ans += n
    realans += ans
    nums = []
for j in range(dimen):
    if j < len(grid[-1]):
        if grid[-1][j]=="+":
            flush()
            op = 0
        elif grid[-1][j]=="*":
            flush()
            op = 1
    assert op is not None
    s=[]
    for row in grid[:-1]:
        if j>=len(row): continue
        och = ord(row[j])-ord("0")
        if 0 <= och and och <= 9:
            s.append(row[j])
    if len(s)>0: nums.append(int("".join(s)))
flush()
print(realans)
