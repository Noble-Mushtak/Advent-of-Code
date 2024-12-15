import sys

ans = 0

st = False
moves = ""
origgrid = []

for line in sys.stdin:
    if line.strip() == "":
        st = True
        continue
    if st:
        moves += line.strip()
    else:
        origgrid.append(list(line.strip()))

N = len(origgrid)
M = len(origgrid[0])
cur_loc = None

grid = [[] for _ in range(N)]
for i in range(N):
    for j in range(M):
        if origgrid[i][j] == "#":
            grid[i].append("#")
            grid[i].append("#")
        elif origgrid[i][j] == "O":
            grid[i].append("[")
            grid[i].append("]")
        elif origgrid[i][j] == ".":
            grid[i].append(".")
            grid[i].append(".")
        elif origgrid[i][j] == "@":
            cur_loc = (i, len(grid[i]))
            print(cur_loc)
            grid[i].append("@")
            grid[i].append(".")

N = len(grid)
M = len(grid[0])
for i in range(N):
    print("".join(grid[i]))

symtodir = {">": (0,1), "<": (0,-1), "^": (-1,0), "v": (1,0)}

def affecting(x,y):
    if grid[x][y] == ".":
        return set()
    elif grid[x][y] == "[":
        return {(x,y), (x, y+1)}
    elif grid[x][y] == "]":
        return {(x,y), (x, y-1)}
    

for move in moves:
    dir = symtodir[move]
    newloc = (cur_loc[0]+dir[0], cur_loc[1]+dir[1])
    if dir[0] == 0 or grid[newloc[0]][newloc[1]] == ".":
        finalloc = newloc
        while grid[finalloc[0]][finalloc[1]] not in {".", "#"}:
            finalloc = (finalloc[0]+dir[0], finalloc[1]+dir[1])

        if grid[finalloc[0]][finalloc[1]] != "#":
            assert grid[newloc[0]][newloc[1]] != "#"
            while (finalloc[0]-dir[0], finalloc[1]-dir[1]) != cur_loc:
                grid[finalloc[0]][finalloc[1]] = grid[finalloc[0]-dir[0]][finalloc[1]-dir[1]]
                finalloc = (finalloc[0]-dir[0], finalloc[1]-dir[1])
            grid[newloc[0]][newloc[1]] = "@"
            grid[cur_loc[0]][cur_loc[1]] = "."
            cur_loc = newloc
    elif grid[newloc[0]][newloc[1]] == "#":
        pass
    else:
        affectings = [affecting(newloc[0],newloc[1])]
        bad = False
        while len(affectings[-1]) > 0:
            new_affect = set()
            for x,y in affectings[-1]:
                if grid[x+dir[0]][y] == "#":
                    bad = True
                    break
                for xd, yd in affecting(x+dir[0], y):
                    new_affect.add((xd, yd))
            if bad:
                break
            affectings.append(new_affect)
            print(affectings)

        if not bad:
            for i in range(len(affectings)-1, -1, -1):
                for x, y in affectings[i]:
                    grid[x+dir[0]][y] = grid[x][y]
                    grid[x][y] = "."
            grid[newloc[0]][newloc[1]] = "@"
            grid[cur_loc[0]][cur_loc[1]] = "."

            print(cur_loc, newloc, grid[newloc[0]][newloc[1]])
            
            cur_loc = newloc
    

for i in range(N):
    print("".join(grid[i]))
    for j in range(M):
        if grid[i][j] == "[":
            ans += 100*i+j
        
print(ans)
