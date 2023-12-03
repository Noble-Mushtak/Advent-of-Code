import sys

grid = []
for line in sys.stdin:
    if line == "": continue
    grid.append(line)

def get_sym(i, j):
    if i < 0 or i >= len(grid) or j < 0 or j >= len(grid[i]): return "."
    ans = grid[i][j]
    if ans == "\n": return "."
    return ans

nums = []
for i, line in enumerate(grid):
    last_was_digit = False
    cur_num = 0
    cur_st = -1
    for j, ch in enumerate(line):
        if ch.isdigit():
            if last_was_digit:
                cur_num *= 10
                cur_num += int(ch)
            else:
                last_was_digit = True
                cur_num = int(ch)
                cur_st = j
        else:
            if last_was_digit:
                nums.append((i, cur_st, j-1, cur_num))
                last_was_digit = False

sum = 0
for row, col_st, col_nd, val in nums:
    print(row, col_st, col_nd, val)
    neighbors = [(row, col_st-1), (row-1, col_st-1), (row+1, col_st-1), (row, col_nd+1), (row-1, col_nd+1), (row+1, col_nd+1)]
    for i in range(col_st, col_nd+1):
        neighbors.append((row-1, i))
        neighbors.append((row+1, i))
    good = False
    for x, y in neighbors:
        if get_sym(x, y) != ".":
            good = True
            break
    if good:
        sum += val
print(sum)
            
