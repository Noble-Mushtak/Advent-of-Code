from itertools import permutations
import copy
import sys
from collections import deque, defaultdict

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
    return keypad[loc[0]][loc[1]] != "X"

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
    def add_node(locs, st, dst, path):
        if not code.startswith(st):
            return
        if (tuple(locs), st) in added:
            return
        added.add((tuple(locs), st))
        bfs_q.append((locs, st, dst, path))
    add_node(locs, "", 0, "")

    ans = None
    ps = []
    
    while True:
        assert len(bfs_q) > 0
        cur_locs, cur_st, cur_dst, cur_path = bfs_q[0]
        #print(cur_locs, cur_st, cur_dst, cur_path)
        bfs_q.popleft()
        assert code.startswith(cur_st)
        
        if code == cur_st:
            return cur_path
        
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
            add_node(lcopy, new_st, cur_dst+1, cur_path+move)


def shorten(moves):
    locs = [(0, 2)]
    out = ""
    for move in moves:
        mv = apply_move([keypad2], locs, move)
        assert mv != False
        if mv != True:
            out += mv
    return out

codels = []
for code in codes:
    codels.append(shorten(shorten(solve(code, [keypad1, keypad2, keypad2, keypad2]))))
    #print(solve(code, [keypad1, keypad2]))
    #print(code, solve(solve(code, [keypad1, keypad2]), [keypad2]))

def to_map(codel):
    cur_map = defaultdict(lambda: 0)
    cur_t = (0,0)
    cur_l = (0,2)
    last_l = (0,2)
    for ch in codel:
        if ch == "A":
            if last_l == (1, 0) and cur_l == (0, 1):
                cur_map[(-1, 1, True)] += 1
            elif last_l == (0, 1) and cur_l == (1, 0):
                cur_map[(1, -1, True)] += 1
            else:
                cur_map[cur_t] += 1
            cur_t = (0,0)
            last_l = cur_l
        x, y = cur_t
        if ch == "<":
            cur_t = (x, y-1)
            cur_l = (cur_l[0], cur_l[1]-1)
        if ch == ">":
            cur_t = (x, y+1)
            cur_l = (cur_l[0], cur_l[1]+1)
        if ch == "v":
            cur_t = (x+1, y)
            cur_l = (cur_l[0]+1, cur_l[1])
        if ch == "^":
            cur_t = (x-1, y)
            cur_l = (cur_l[0]-1, cur_l[1])
    return cur_map
    
transforms = {}
transformr = {}
transformc = {}

def process1(x, y, spec=False):
    code = ""
    for i in range(abs(x)):
        if x < 0: code += "^"
        else: code += "v"
        
    for i in range(abs(y)):
        if y < 0: code += "<"
        else: code += ">"

    mn_code = code
    mn_res = float("inf")
    for perm in permutations(code):
        if "".join(perm) == "<<v" or "".join(perm) == "^>>" or (spec and ("".join(perm) == "^>" or "".join(perm) == "<v")):
            continue
        cur_res = len(solve("".join(perm)+"A", [keypad2]))
        if cur_res < mn_res:
            mn_code = perm
            mn_res = cur_res
    res = solve("".join(mn_code)+"A", [keypad2])
    if spec:
        transformc[(x, y, spec)] = len(res)
    else:
        transformc[(x, y)] = len(res)

for x, y in [(x, y) for x in [0,1,-1] for y in [0,1,2,-1,-2]]:
    process1(x, y, False)
process1(1, -1, True)
process1(-1, 1, True)

def process2(x, y, spec=False):
    code = ""
    for i in range(abs(x)):
        if x < 0: code += "^"
        else: code += "v"
        
    for i in range(abs(y)):
        if y < 0: code += "<"
        else: code += ">"

    mn_code = code
    mn_res = float("inf")
    for perm in permutations(code):
        if "".join(perm) == "<<v" or "".join(perm) == "^>>" or (spec and ("".join(perm) == "^>" or "".join(perm) == "<v")):
            continue
        cur_map = to_map(solve("".join(perm)+"A", [keypad2]))
        cur_res = 0
        for tpl, i in cur_map.items():
            cur_res += transformc[tpl]*i
        if cur_res < mn_res:
            mn_code = perm
            mn_res = cur_res
    print(x, y, mn_code)
    #if mn_res > transformc[(x, y)]:
    #    print("E", x, y, mn_res, len(solve("".join(mn_code)+"A", [keypad2])), solve("".join(mn_code)+"A", [keypad2]))
    res = solve("".join(mn_code)+"A", [keypad2])
    if spec:
        transforms[(x, y, spec)] = to_map(res)
        transformr[(x, y, spec)] = res
    else:
        transforms[(x, y)] = to_map(res)
        transformr[(x, y)] = res
    
for x, y in [(x, y) for x in [0,1,-1] for y in [0,1,2,-1,-2]]:
    process2(x, y)
process2(1, -1, True)
process2(-1, 1, True)

transform_maps = []
for codel in codels:
    transform_maps.append(to_map(codel))
    
for tpl, i in transforms.items():
    print(tpl, dict(i))
print(transformc)

for i in range(23):
    new_maps = []
    for cur_map in transform_maps:
        new_map = defaultdict(lambda: 0)
        for tpl, i in cur_map.items():
            for tpl2, i2 in transforms[tpl].items():
                new_map[tpl2] += i*i2
        new_maps.append(new_map)
    transform_maps = new_maps
    
ans = 0
for i, code in enumerate(codes):
    l = 0
    #transform_maps[i] = dict(to_map(shorten(solve(code, [keypad1, keypad2, keypad2, keypad2]))))
    for tpl, j in transform_maps[i].items():
        #print(tpl, transformc[tpl], j, transformr[tpl])
        l += transformc[tpl]*j
    print(l, code, dict(to_map(shorten(solve(code, [keypad1, keypad2, keypad2])))))
    print(l, code, dict(to_map(shorten(shorten(solve(code, [keypad1, keypad2, keypad2, keypad2]))))))
    print()
    print(l, dict(transform_maps[i]))
    print(l, code, dict(to_map(shorten(solve(code, [keypad1, keypad2, keypad2, keypad2])))))
    print()
    print(l, code, shorten(solve(code, [keypad1, keypad2, keypad2, keypad2])))
    print(l, code, solve(code, [keypad1, keypad2, keypad2, keypad2]))
    print()
    ans += l * int(code[:-1])

print(ans)

