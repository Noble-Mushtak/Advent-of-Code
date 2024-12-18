import sys
from collections import deque

ans = 0

N = 71
grid = [[False for _ in range(N)] for _ in range(N)]

points = []

so_far = 0

for line in sys.stdin:
    if line.strip() == "":
        continue
    a,b = map(int,line.strip().split(","))
    points.append((a,b))

limit = 0
while True:
    grid[points[limit][0]][points[limit][1]] = True
    limit += 1
    
    st = (0,0)
    dst = {(0,0): 0}
    bfs_q = deque()
    bfs_q.append((0,0))

    good = False
    while len(bfs_q) > 0:
        cur_x, cur_y = bfs_q[0]
        bfs_q.popleft()
        if cur_x == N-1 and cur_y == N-1:
            good = True
            break

        for dx, dy in [(0,1),(0,-1),(1,0),(-1,0)]:
            new_x, new_y = cur_x+dx, cur_y+dy
            if 0 <= new_x < N and 0 <= new_y < N and not grid[new_x][new_y] and (new_x,new_y) not in dst:
                dst[(new_x,new_y)] = dst[(cur_x,cur_y)]+1
                bfs_q.append((new_x,new_y))

    if not good:
        print(limit)
        print(points[limit-1][0], points[limit-1][1])
        break
                
