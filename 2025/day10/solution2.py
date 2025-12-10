import itertools
import sys
from collections import deque
from fractions import Fraction

sumans = 0

for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    toks = line.split()

    tools = []
    for tok in toks[1:-1]:
        tools.append(list(map(int, tok[1:-1].split(","))))

    exp = list(map(int, toks[-1][1:-1].split(",")))
    max_exp = max(exp)

    matrix = [[Fraction(0,1) for _ in range(len(tools)+1)] for _ in range(len(exp))]
    for i, exp_val in enumerate(exp):
        matrix[i][-1] = Fraction(exp_val, 1)
    for i, tool in enumerate(tools):
        for num in tool:
            matrix[num][i] = Fraction(1,1)
    row_idx = 0
    free_vars = []
    for i in range(len(tools)):
        next_row = None
        for j, row in enumerate(matrix):
            if j < row_idx: continue
            if row[i] != Fraction(0,1):
                next_row = j
                break
        if next_row is None:
            free_vars.append(i)
            continue
        matrix[row_idx], matrix[next_row] = matrix[next_row], matrix[row_idx]
        for k, row in enumerate(matrix):
            if k == row_idx:
                continue
            orig_row_i = row[i]
            for j, cell in enumerate(row):
                row[j] = cell - matrix[row_idx][j] * orig_row_i / matrix[row_idx][i]
        row_idx += 1

    for row in matrix:
        for i, cell in enumerate(row):
            if cell == int(cell):
                row[i] = int(cell)
    
    realans = None
    for tuple in itertools.product(*[range(max_exp+1) for _ in free_vars]):
        good = True
        ans = sum(tuple)
        if realans is not None and ans > realans:
            continue
        for row in matrix:
            if min(row) == 0 and max(row) == 0:
                continue
            subans = row[-1]
            for i, j in enumerate(free_vars):
                subans -= row[j] * tuple[i]
            first_nonzero = 0
            for el in row:
                if el != 0:
                    first_nonzero = el
                    break
            if type(subans) == int and type(first_nonzero) == int:
                if subans % first_nonzero != 0:
                    good = False
                    break
                subans = subans // first_nonzero
            else:
                subans /= first_nonzero
                if subans != int(subans):
                    good = False
                    break
                subans = int(subans)
            if subans < 0:
                good = False
                break
            # print(tuple, free_vars, list(map(float, row)), subans)
            ans += subans
            if realans is not None and ans > realans:
                break
        if good:
            # print(tuple, ans)
            if realans is None:
                realans = ans
            else:
                realans = min(realans, ans)
    print(realans)
    sumans += realans
    
print(sumans)
