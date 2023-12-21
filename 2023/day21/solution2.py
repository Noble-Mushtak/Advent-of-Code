import sys
from collections import defaultdict, deque

grid = []
st_loc = None
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    if "S" in line:
        st_loc = (len(grid), line.index("S"))
    grid.append(line)

STEPS = 26501365

def mn_dst(st_loc):
    mp = {}
    mp[st_loc] = 0
    bfs_q = deque()
    bfs_q.append(st_loc)
    while len(bfs_q) > 0:
        x, y = bfs_q.popleft()
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx, ny = x+dx, y+dy
            if (nx, ny) not in mp and nx >= 0 and nx < len(grid) and ny >= 0 and ny < len(grid[0]) and grid[nx][ny] != "#":
                mp[(nx, ny)] = mp[(x, y)] + 1
                bfs_q.append((nx, ny))
    return mp

ans = 0

starters = mn_dst(st_loc)
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if (i, j) not in starters: continue
        dst = starters[(i, j)]
        if dst % 2 == STEPS % 2 and dst <= STEPS:
            ans += 1

posxs = mn_dst((0, st_loc[1]))
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if (i, j) not in posxs: continue
        dst = posxs[(i, j)] + len(grid) - st_loc[0]
        if dst % 2 == STEPS % 2 and dst <= STEPS:
            # What is biggest x such that dst + len(grid)*2*x <= STEPS
            # len(grid) * 2 * x <= STEPS - dst
            ans += (STEPS - dst) // (2 * len(grid)) + 1
        if dst % 2 != STEPS % 2 and dst + len(grid) <= STEPS:
            # What is biggest x such that dst + len(grid)*x <= STEPS
            # len(grid) * (2 * x + 1) <= STEPS - dst
            ans += (STEPS - dst - len(grid)) // (2 * len(grid)) + 1

posxs = mn_dst((len(grid)-1, st_loc[1]))
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if (i, j) not in posxs: continue
        dst = posxs[(i, j)] + st_loc[0] + 1
        if dst % 2 == STEPS % 2 and dst <= STEPS:
            ans += (STEPS - dst) // (2 * len(grid)) + 1
        if dst % 2 != STEPS % 2 and dst + len(grid) <= STEPS:
            ans += (STEPS - dst - len(grid)) // (2 * len(grid)) + 1

posxs = mn_dst((st_loc[0], 0))
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if (i, j) not in posxs: continue
        dst = posxs[(i, j)] + len(grid[0]) - st_loc[1]
        if dst % 2 == STEPS % 2 and dst <= STEPS:
            ans += (STEPS - dst) // (2 * len(grid[0])) + 1
        if dst % 2 != STEPS % 2 and dst + len(grid[0]) <= STEPS:
            ans += (STEPS - dst - len(grid[0])) // (2 * len(grid[0])) + 1

posxs = mn_dst((st_loc[0], len(grid[0])-1))
for i in range(len(grid)):
    for j in range(len(grid[0])):
        if (i, j) not in posxs: continue
        dst = posxs[(i, j)] + st_loc[1] + 1
        if dst % 2 == STEPS % 2 and dst <= STEPS:
            ans += (STEPS - dst) // (2 * len(grid[0])) + 1
        if dst % 2 != STEPS % 2 and dst + len(grid[0]) <= STEPS:
            ans += (STEPS - dst - len(grid[0])) // (2 * len(grid[0])) + 1

for stx in [0, len(grid)-1]:
    for sty in [0, len(grid[0])-1]:
        posxs = mn_dst((stx, sty))
        dstx = len(grid) - st_loc[0] if st_loc[0] == 0 else st_loc[0] + 1
        dsty = len(grid[0]) - st_loc[1] if st_loc[1] == 0 else st_loc[1] + 1
        for i in range(len(grid)):
            for j in range(len(grid[0])):
                if (i, j) not in posxs: continue
                dst = posxs[(i, j)] + dstx + dsty
                if dst % 2 == STEPS % 2 and dst <= STEPS:
                    ans += ((STEPS - dst) // (2 * len(grid[0])) + 1) ** 2
                if dst % 2 != STEPS % 2 and dst + len(grid[0]) <= STEPS:
                    x = (STEPS - dst - len(grid[0])) // (2 * len(grid[0])) + 1
                    ans += x * (x+1)

print(ans)

