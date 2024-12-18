import sys
from collections import deque

limit = 1024
ans = 0

N = 71
grid = [[False for _ in range(N)] for _ in range(N)]

so_far = 0

for line in sys.stdin:
    if line.strip() == "":
        continue
    a,b = map(int,line.strip().split(","))
    so_far += 1
    if so_far <= limit:
        grid[a][b] = True

st = (0,0)
dst = {(0,0): 0}
bfs_q = deque()
bfs_q.append((0,0))
while len(bfs_q) > 0:
    cur_x, cur_y = bfs_q[0]
    bfs_q.popleft()
    if cur_x == N-1 and cur_y == N-1:
        print(dst[(cur_x, cur_y)])
        break
    print(cur_x,cur_y)

    for dx, dy in [(0,1),(0,-1),(1,0),(-1,0)]:
        new_x, new_y = cur_x+dx, cur_y+dy
        if 0 <= new_x < N and 0 <= new_y < N and not grid[new_x][new_y] and (new_x,new_y) not in dst:
            dst[(new_x,new_y)] = dst[(cur_x,cur_y)]+1
            bfs_q.append((new_x,new_y))

print("DONE")
