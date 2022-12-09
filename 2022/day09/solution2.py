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

knots = [(0, 0) for _ in range(10)]
vis = set()
vis.add(knots[0])

def calc_tail(head, tail):
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
    return tail
        
for dx_, dy_ in changes:
    knots[-1] = (knots[-1][0]+dx_, knots[-1][1]+dy_)
    for i in range(len(knots)-2, -1, -1):
        knots[i] = calc_tail(knots[i+1], knots[i])
    vis.add(knots[0])
    
print(len(vis))
