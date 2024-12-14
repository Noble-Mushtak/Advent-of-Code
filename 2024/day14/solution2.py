import sys
import time

W = 101
H = 103

ans = [0,0,0,0]

robots = []

st = 0
for line in sys.stdin:
    if line.strip() == "":
        continue
    p,v = line.split()
    px,py = map(int,p[2:].split(",")) 
    vx,vy = map(int,v[2:].split(","))
    robots.append(((px,py),(vx,vy)))

seconds = 0
while True:
    grid = [[0 for _ in range(W)] for _ in range(H)]
    seconds += 1

    bad = False
    for robot in robots:
        pr1,pr2 = robot
        px,py = pr1
        vx,vy = pr2
        nx,ny = px + seconds*vx, py + seconds*vy
        nx = nx % W
        ny = ny % H
        grid[ny][nx] += 1
        if grid[ny][nx] > 1:
            bad = True

    if not bad:
        print(seconds)
        for row in grid:
            print("".join(map(str,row)))
        time.sleep(0.3)
