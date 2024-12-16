import sys
from heapq import *

ans = 0
grid = []

for line in sys.stdin:
    if line.strip() == "":
        continue
    grid.append(line.strip())

N = len(grid)
M = len(grid[0])

cur_loc = None
nd_loc = None
for i in range(N):
    for j in range(M):
        if grid[i][j] == "S":
            cur_loc = (i,j)
        if grid[i][j] == "E":
            nd_loc = (i,j)

def solve(cur_st, op_sign=False):
    dijk_q = [(0, cur_st)]
    d_map = {cur_st: 0}

    def add_node(new_st, new_d):
        if new_st not in d_map or d_map[new_st] > new_d:
            d_map[new_st] = new_d
            heappush(dijk_q, (new_d, new_st))

    while len(dijk_q) > 0:
        cur_d, cur_st = heappop(dijk_q)

        if d_map[cur_st] < cur_d:
            continue

        cur_p, cur_dir = cur_st
        sgn = -1 if op_sign else 1
        new_p = (cur_p[0]+sgn*cur_dir[0], cur_p[1]+sgn*cur_dir[1])
        if 0 <= new_p[0] < N and 0 <= new_p[1] < M and grid[new_p[0]][new_p[1]] != "#":
            new_st = (new_p, cur_dir)
            new_d = cur_d+1
            add_node(new_st, new_d)

        other_dirs = [(1,0),(-1,0)]
        if cur_dir in other_dirs:
            other_dirs = [(0,1),(0,-1)]
        for other_dir in other_dirs:
            add_node((cur_p, other_dir), cur_d+1000)

    return d_map

init_solve = solve((cur_loc, (0,1)))
ans = min([init_solve[(nd_loc, dir)] for dir in [(0,1),(0,-1),(1,0),(-1,0)]])
print(ans)

nd_solves = [solve((nd_loc, dir), True) for dir in [(0,1),(0,-1),(1,0),(-1,0)]]
#print(init_solve[(cur_loc,(0,1))], nd_solves[2][(cur_loc,(0,1))])
origans = ans
ansset = set()
for i in range(N):
    for j in range(M):
        for dir in [(0,1),(0,-1),(1,0),(-1,0)]:
            cur_st = ((i,j),dir)
            good = False
            for nd_solve in nd_solves:
                if cur_st not in init_solve or cur_st not in nd_solve:
                    continue
                tryans = init_solve[cur_st] + nd_solve[cur_st]
                assert tryans >= origans
                if tryans == origans:
                    print(cur_st)
                    good = True
                    break
            if good:
                ansset.add(cur_st[0])
#print(cur_loc)
print(len(ansset))
