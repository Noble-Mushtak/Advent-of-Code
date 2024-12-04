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
for strr in range(len(grid)):
    for stc in range(len(grid[0])):
        for dirr, dirc in dirs:
            if get_ch(strr, stc) == "X" and get_ch(strr+dirr, stc+dirc) == "M" and get_ch(strr+2*dirr, stc+2*dirc) == "A" and get_ch(strr+3*dirr, stc+3*dirc) == "S":
                ans += 1


print(ans)
