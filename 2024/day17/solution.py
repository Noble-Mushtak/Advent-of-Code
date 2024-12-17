import sys
from heapq import *

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

def interp_combo(regs, val):
    if val < 4: return val
    return regs[val - 4] if val < 7 else None
        
def run_program(regs, prog):
    outt = []
    inst_ptr = 0
    while inst_ptr < len(prog):
        print(inst_ptr, regs)
        code = prog[inst_ptr]
        op = prog[inst_ptr + 1]
        combo_op = interp_combo(regs, op)
        jumped = False

        if code == 0:
            regs[0] = regs[0] // (2**combo_op)
        elif code == 1:
            regs[1] = regs[1] ^ op
        elif code == 2:
            regs[1] = combo_op % 8
        elif code == 3:
            if regs[0] != 0:
                inst_ptr = op
                jumped = True
        elif code == 4:
            regs[1] = regs[1] ^ regs[2]
        elif code == 5:
            outt.append(combo_op % 8)
        elif code == 6:
            regs[1] = regs[0] // (2**combo_op)
        elif code == 7:
            print(op, combo_op)
            regs[2] = regs[0] // (2**combo_op)
        if not jumped:
            inst_ptr += 2
    return outt

print(",".join(map(str,run_program(registers, program))))
