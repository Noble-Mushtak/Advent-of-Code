import sys
from collections import deque

inpt = None
for line in sys.stdin:
    if line.strip() == "": continue
    inpt = line.strip()

filled = []
curid = 0
first_gap = None
place = True
for ch in inpt:
    x = int(ch)
    if place:
        filled.append((curid, x))
    else:
        if first_gap is None:
            first_gap = len(filled)
        filled.append((-1, x))
    if place:
        curid += 1
    place = not place

id_to_move = curid-1
while id_to_move >= 0:
    top_idx = len(filled)-1
    while filled[top_idx][0] != id_to_move:
        top_idx -= 1
    for i in range(top_idx):
        if filled[i][0] == -1 and filled[i][1] >= filled[top_idx][1]:
            space_left = filled[i][1]-filled[top_idx][1]
            filled = filled[:i] + [filled[top_idx], (-1, space_left)] + filled[(i+1):top_idx] + [(-1, filled[top_idx][1])] + filled[(top_idx+1):]
            break
    id_to_move -= 1
    new_filled = []
    cur_space = 0
    for a, b in filled:
        if a != -1:
            if cur_space > 0:
                new_filled.append((-1, cur_space))
            cur_space = 0
            new_filled.append((a, b))
        else:
            cur_space += b
    filled = new_filled
            

ans = 0
loc = 0
for a, b in filled:
    if a != -1:
        for x in range(b):
            ans += loc*a
            loc += 1
    else:
        loc += b

print(ans)
