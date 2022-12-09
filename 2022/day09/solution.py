import sys

dirs = {
    "R": (1, 0),
    "L": (-1, 0),
    "U": (0, -1),
    "D": (0, 1)
    }
changes = []
for line in sys.stdin:
    dir, num = line.split()
    for _ in range(int(num)):
        changes.append(dirs[dir])

tail = (0, 0)
head = (0, 0)
vis = set()
vis.add(tail)
        
for dx_, dy_ in changes:
    head = (head[0]+dx_, head[1]+dy_)
    diff = (head[0]-tail[0], head[1]-tail[1])
    if diff[0] != 0 and diff[1] != 0:
        if abs(diff[0]) > 1 or abs(diff[1]) > 1:
            dx = 1
            if diff[0] < 0:
                dx = -1
            dy = 1
            if diff[1] < 0:
                dy = -1
            tail = (tail[0]+dx, tail[1]+dy)
    elif diff[0] == 0:
        if abs(diff[1]) > 1:
            dy = 1
            if diff[1] < 0:
                dy = -1
            tail = (tail[0], tail[1]+dy)
    else:
        if abs(diff[0]) > 1:
            dx = 1
            if diff[0] < 0:
                dx = -1
            tail = (tail[0]+dx, tail[1])
    vis.add(tail)
    
print(len(vis))
