import sys
from heapq import *

from z3 import *

ans = 0

"""
adv - 0 - A / combo() -> A
bxl - 1 - B ^ lit() -> B
bst - 2 - combo() % 8 -> B
jnz - 3 - if A != 0 { jmp literal() }
bxc - 4 - B ^ C -> B (ignore operand)
out - 5 - output combo()
bdv - 6 - A / combo() -> B
cdv - 7 - A / combo() -> C
"""

st = False
registers = []
program = None

for line in sys.stdin:
    if line.strip() == "":
        st = True
        continue
    if not st:
        registers.append(int(line.split(": ")[1]))
    else:
        program = list(map(int,line.split(": ")[1].split(",")))

s = Solver()
A = BitVec('var', 3*len(program)+1)
for i in range(len(program)):
    B = ((A >> (3*i)) & 7) ^ 2
    C = A >> (3*i+B)
    Bp = B ^ 7
    s.add((Bp ^ C) & 7 == program[i])
    #     190593310997519
s.add(A < 190593310997520)
    
if s.check() == sat:
    B = ((14 >> (3*0)) & 7) ^ 5
    print(B)
    C = 14 >> (3*0+B)
    print(C)
    print(s.model()[A].as_long())
