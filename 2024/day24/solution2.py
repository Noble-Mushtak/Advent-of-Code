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
        
@cache
def inspect(op, depth=0):
    if op in values or depth >= 3:
        return op
    
    op1 = inspect(gates[op][0], depth+1)
    op2 = inspect(gates[op][2], depth+1)
    if gates[op][1] == "AND":
        return f"{op}{{({op1}) & ({op2})}}"
    if gates[op][1] == "OR":
        return f"{op}{{({op1}) | ({op2})}}"
    if gates[op][1] == "XOR":
        return f"{op}{{({op1}) ^ ({op2})}}"

def get_num(beg):
    digs = {}
    for op in all_ops:
        if op.startswith(beg):
            digs[int(op[1:])] = evaluate(op)

    x = max([k for k in digs])
    num = 0
    for i in range(x,-1,-1):
        num = 2*num + digs[i]
    return num

# Code I used to figure out which bits were wrong:
x = get_num("x")
y = get_num("y")
z = get_num("z")
print(bin(x+y))
print(bin(z))
#assert x+y ==z

# Code I used to figure out how bit 8 was wrong:
print(inspect("z00"))
print(inspect("z01"))
print(inspect("z02"))
print(inspect("z03"))
print(inspect("z04"))
print(inspect("z05"))
print(inspect("z06"))
print(inspect("z07"))
print(inspect("z08"))
print(inspect("z09"))

# Code I used to figure out how bit 16 was wrong:
print(inspect("z14"))
print(inspect("z15"))
print(evaluate("y15"), evaluate("x15"), evaluate("sfc"), evaluate("sdv"), evaluate("x15"), evaluate("y15"), evaluate("y16"), evaluate("x16"))
print(evaluate("rnq"))
print(evaluate("z16"))
print(inspect("z16"))

# Code I used to figure out how bit 26 was wrong:
print(inspect("z26"))
print(inspect("z27"))
print(inspect("z28"))

# Code I used to figure out how bit 39 was wrong:
print(inspect("z37"))
print(inspect("z38"))
print(inspect("z39"))
