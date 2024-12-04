import sys
ans = 0
grid = []
for line in sys.stdin:
    if line.strip() == "": continue
    grid.append(line)

def get_ch(r, c):
    if r < 0: return "N"
    if r >= len(grid): return "N"
    if c < 0: return "N"
    if c >= len(grid[r]): return "N"
    return grid[r][c]

dirs = [(1, 0), (0, 1), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
for strr in range(1, len(grid)-1):
    for stc in range(1, len(grid[0])-1):
        if grid[strr][stc] == "A":
            if {grid[strr-1][stc-1], grid[strr+1][stc+1]} == {"M", "S"} and {grid[strr-1][stc+1], grid[strr+1][stc-1]} == {"M", "S"}:
                ans += 1


print(ans)
