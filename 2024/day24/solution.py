import copy
import sys
from collections import deque, defaultdict
from functools import cache

ans = 0
st = False

all_ops = set()
values = {}
gates = {}

for line in sys.stdin:
    if line.strip() == "":
        st = True
        continue
    if st:
        op1, op, op2, arrow, out_op = line.strip().split()
        gates[out_op] = (op1, op, op2)
        all_ops.add(out_op)
    else:
        op, val = line.strip().split()
        values[op[:-1]] = int(val)
        all_ops.add(op[:-1])

@cache
def evaluate(op):
    if op in values:
        return values[op]
    op1 = evaluate(gates[op][0])
    op2 = evaluate(gates[op][2])
    if gates[op][1] == "AND":
        return op1 & op2
    if gates[op][1] == "OR":
        return op1 | op2
    if gates[op][1] == "XOR":
        return op1 ^ op2

digs = {}
for op in all_ops:
    if op.startswith("z"):
        digs[int(op[1:])] = evaluate(op)

x = max([k for k in digs])
num = 0
for i in range(x,-1,-1):
    num = 2*num + digs[i]
print(num)
