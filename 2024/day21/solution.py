import copy
import sys
from collections import deque

ans = 0

codes = []

for line in sys.stdin:
    if line.strip() == "":
        continue

    codes.append(line.strip())

keypad1 = [["7", "8", "9"], ["4", "5", "6"], ["1", "2", "3"], ["X", "0", "A"]]
keypad2 = [["X", "^", "A"], ["<", "v", ">"]]

def is_valid(idx, keypad, loc):
    if not (0 <= loc[0] < len(keypad)):
        return False
    if not (0 <= loc[1] < len(keypad[loc[0]])):
        return False
    if idx > 0:
        return keypad[loc[0]][loc[1]] in "A<>^v"
    else:
        return keypad[loc[0]][loc[1]] in "0123456789A"

def apply_move(keypads, locs, move):
    for idx in range(len(keypads)-1, -1, -1):
       if move == "<":
           locs[idx] = (locs[idx][0], locs[idx][1]-1)
           return is_valid(idx, keypads[idx], locs[idx])
       elif move == ">":
           locs[idx] = (locs[idx][0], locs[idx][1]+1)
           return is_valid(idx, keypads[idx], locs[idx])
       elif move == "v":
           locs[idx] = (locs[idx][0]+1, locs[idx][1])
           return is_valid(idx, keypads[idx], locs[idx])
       elif move == "^":
           locs[idx] = (locs[idx][0]-1, locs[idx][1])
           return is_valid(idx, keypads[idx], locs[idx])
       else:
           assert move == "A"
           assert is_valid(idx, keypads[idx], locs[idx])
           move = keypads[idx][locs[idx][0]][locs[idx][1]]
           if idx == 0:
               return move

def solve(code, keypads):
    N = len(keypads)
    locs = []
    for l in range(N):
        new_loc = None
        for i, row in enumerate(keypads[l]):
            for j, ch in enumerate(row):
                if ch == "A":
                    new_loc = (i,j)
                    break
        locs.append(new_loc)

    bfs_q = deque()
    added = set()
    def add_node(locs, st, dst):
        if not code.startswith(st):
            return
        if (tuple(locs), st) in added:
            return
        added.add((tuple(locs), st))
        bfs_q.append((locs, st, dst))
    add_node(locs, "", 0)

    while True:
        assert len(bfs_q) > 0
        cur_locs, cur_st, cur_dst = bfs_q[0]
        #print(cur_locs, cur_st, cur_dst)
        bfs_q.popleft()
        assert code.startswith(cur_st)
        assert cur_st != code
        for move in "A<>^v":
            lcopy = copy.deepcopy(cur_locs)
            rt = apply_move(keypads, lcopy, move)
            if rt == False:
                #print(lcopy, move)
                continue
            
            new_st = cur_st
            if rt != True:
                new_st += rt
            #print("D", rt, new_st)
            if new_st == code:
                return cur_dst+1
            add_node(lcopy, new_st, cur_dst+1)

lcopy = [(3, 2), (1, 0), (0, 2)]
print(apply_move([keypad1, keypad2, keypad2], lcopy, "A"))
print(lcopy)

for code in codes:
    l = solve(code, [keypad1, keypad2, keypad2])
    print(code, l)
    ans += l * int(code[:-1])

print(ans)
