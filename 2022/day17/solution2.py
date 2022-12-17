import sys
import json
sys.setrecursionlimit(3000000)

offsets = [
    [(0, 0), (1, 0), (2, 0), (3, 0)],
    [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (0, 1), (1, 0), (1, 1)]
]
MNX = 0
MXX = 6

def gen_pts(x, y, offs):
    pts = []
    for a, b in offs:
        pts.append((x+a, y+b))
    return pts

commands = sys.stdin.read().strip()
dirs = []
for comm in commands:
    if comm == "<":
        dirs.append(-1)
    else:
        dirs.append(1)

cur_mxy = 0
rocks = set()

def check_valid(pts):
    for x, y in pts:
        if (x, y) in rocks:
            return False
        if x < MNX or x > MXX:
            return False
        if y <= 0:
            return False
    return True

dir_idx = 0
mxys = [0 for _ in range(MXX+1)]
infos = {}
total_rocks = 1000000000000
reset = False
history = []
i = 0
to_add = 0
while i < total_rocks:
    # print(i, total_rocks)
    startx = 2
    starty = cur_mxy+4
    cur_offs = offsets[i % 5]
    while True:
        # print(i, startx, starty, dirs[dir_idx])
        new_pts = gen_pts(startx+dirs[dir_idx], starty, cur_offs)
        if check_valid(new_pts):
            startx += dirs[dir_idx]
        dir_idx += 1
        dir_idx %= len(dirs)
        
        new_pts = gen_pts(startx, starty-1, cur_offs)
        if check_valid(new_pts):
            starty -= 1
        else:
            break
    # print("F", i, startx, starty)
    for x, y in gen_pts(startx, starty, cur_offs):
        rocks.add((x, y))
        cur_mxy = max(cur_mxy, y)
        mxys[x] = max(mxys[x], y)
    if not reset:
        cur_info = (dir_idx, tuple([cur_mxy-mxys[z] for z in range(MXX+1)]))
        if cur_info in infos:
            old_idx = infos[cur_info]
            to_add = (total_rocks-i)//(i-old_idx)*(cur_mxy-history[old_idx])
            total_rocks -= ((total_rocks-i)//(i-old_idx))*(i-old_idx)
            reset = True
        else:
            infos[cur_info] = i
            history.append(cur_mxy)
    i += 1
print(cur_mxy+to_add)
