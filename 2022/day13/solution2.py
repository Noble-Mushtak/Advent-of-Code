import sys
import json
from functools import cmp_to_key
sys.setrecursionlimit(3000000)

def compare_vals(l1, l2):
    if type(l1) == int:
        if type(l2) == int:
            if l1 < l2:
                return -1
            elif l1 > l2:
                return 1
            return 0
        else:
            l1 = [l1]
    if type(l2) == int:
        if type(l1) == int:
            if l1 < l2:
                return -1
            elif l1 > l2:
                return 1
            return 0
        else:
            l2 = [l2]
            
    for i in range(min(len(l1), len(l2))):
        res = compare_vals(l1[i], l2[i])
        if res != 0:
            return res
    if len(l1) < len(l2):
        return -1
    if len(l1) > len(l2):
        return 1
    return 0

x = sys.stdin.read().split("\n\n")
ans = 0
vs = []
for i, l in enumerate(x):
    ws = l.split("\n")
    if len(ws) >= 2:
        try:
            l1 = json.loads(ws[0])
            l2 = json.loads(ws[1])
            vs.append(l1)
            vs.append(l2)
        except json.decoder.JSONDecodeError:
            pass

vs.append([[2]])
vs.append([[6]])
vs.sort(key=cmp_to_key(compare_vals))
print((vs.index([[2]])+1)*(vs.index([[6]])+1))
