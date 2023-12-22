import sys
from collections import defaultdict, deque

allblocks = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    a, b = line.split("~")
    ax, ay, az = list(map(int, a.split(",")))
    bx, by, bz = list(map(int, b.split(",")))
    if az > bz:
        ax, ay, az, bx, by, bz = bx, by, bz, ax, ay, az
    # az <= bz
    zs = (az, bz)
    blocks = None
    if ax == bx:
        if ay <= by:
            blocks = [(ax, y) for y in range(ay, by+1)]
        else:
            # ay > by
            blocks = [(ax, y) for y in range(by, ay+1)]
    else:
        assert(ay == by)
        if ax <= bx:
            blocks = [(x, ay) for x in range(ax, bx+1)]
        else:
            blocks = [(x, ay) for x in range(bx, ax+1)]
    allblocks.append((blocks, zs))

allblocks.sort(key=lambda tpl: tpl[1][0])
lowest = defaultdict(lambda: (-1, -1))
supporting = defaultdict(set)
supported_by = defaultdict(set)
for i, tpl in enumerate(allblocks):
    blocks, zs = tpl
    lowest_pt = 0
    for block in blocks:
        lowest_pt = max(lowest_pt, lowest[block][0]+1)
    highest_pt = lowest_pt + zs[1] - zs[0]
    for block in blocks:
        if lowest[block][0]+1 == lowest_pt and lowest[block][1] != -1:
            supporting[i].add(lowest[block][1])
            supported_by[lowest[block][1]].add(i)
        lowest[block] = (highest_pt, i)

ans = 0
for block in range(len(allblocks)):
    bad = False
    for supported in supported_by[block]:
        if len(supporting[supported]) == 1:
            bad = True
            break
    ans += int(not bad)

print(ans)
