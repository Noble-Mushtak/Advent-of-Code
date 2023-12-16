import sys
from collections import defaultdict, deque

grid = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid.append(line)

DIRS = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    
adj = defaultdict(list)
for dy, dx in DIRS:
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            if grid[i][j] == ".":
                adj[((i, j), (dy, dx))] = [((i+dy, j+dx), (dy, dx))]
            elif grid[i][j] == "\\":
                adj[((i, j), (dy, dx))] = [((i+dx, j+dy), (dx, dy))]
            elif grid[i][j] == "/":
                adj[((i, j), (dy, dx))] = [((i-dx, j-dy), (-dx, -dy))]
            elif grid[i][j] == "|":
                if dx == 0:
                    adj[((i, j), (dy, dx))] = [((i+dy, j+dx), (dy, dx))]
                else:
                    adj[((i, j), (dy, dx))] = [((i+1, j), (1, 0)), ((i-1, j), (-1, 0))]
            else:
                if dy == 0:
                    adj[((i, j), (dy, dx))] = [((i+dy, j+dx), (dy, dx))]
                else:
                    adj[((i, j), (dy, dx))] = [((i, j+1), (0, 1)), ((i, j-1), (0, -1))]

vis = set()
st = ((0, 0), (0, 1))
bfs_q = deque()
bfs_q.append(st)
vis.add(st)
while len(bfs_q) > 0:
    cur_st = bfs_q.popleft()
    print(cur_st)
    for next_st in adj[cur_st]:
        if next_st not in vis and next_st[0][0] >= 0 and next_st[0][0] < len(grid) and next_st[0][1] >= 0 and next_st[0][1] < len(grid[0]):
            bfs_q.append(next_st)
            vis.add(next_st)
ans = set()
for a, b in vis:
    ans.add(a)
print(len(ans))
