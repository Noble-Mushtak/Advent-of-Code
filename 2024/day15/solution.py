import sys

ans = 0

st = False
moves = ""
grid = []

for line in sys.stdin:
    if line.strip() == "":
        st = True
        continue
    if st:
        moves += line.strip()
    else:
        grid.append(list(line.strip()))

N = len(grid)
M = len(grid[0])
        
cur_loc = None
for i in range(N):
    for j in range(M):
        if grid[i][j] == "@":
            cur_loc = (i,j)
            break

symtodir = {">": (0,1), "<": (0,-1), "^": (-1,0), "v": (1,0)}

for move in moves:
    dir = symtodir[move]
    newloc = (cur_loc[0]+dir[0], cur_loc[1]+dir[1])
    finalloc = newloc
    while grid[finalloc[0]][finalloc[1]] not in {".", "#"}:
        finalloc = (finalloc[0]+dir[0], finalloc[1]+dir[1])

    if grid[finalloc[0]][finalloc[1]] != "#":
        assert grid[newloc[0]][newloc[1]] != "#"
        grid[finalloc[0]][finalloc[1]] = grid[newloc[0]][newloc[1]]
        grid[newloc[0]][newloc[1]] = "@"
        grid[cur_loc[0]][cur_loc[1]] = "."
        cur_loc = newloc

for i in range(N):
    print("".join(grid[i]))
    for j in range(M):
        if grid[i][j] == "O":
            ans += 100*i+j
        
print(ans)
