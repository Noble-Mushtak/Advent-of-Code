import sys
from collections import defaultdict, deque

workflows = {}
rinputs = False
inputs = []
for line in sys.stdin:
    line = line.strip()
    if line == "":
        rinputs = True
        continue
    if rinputs:
        parts = line[1:-1].split(",")
        d = {}
        for part in parts:
            a, b = part.split("=")
            d[a] = int(b)
        inputs.append(d)
    else:
        name, p = line.split("{")
        answers = []
        for part in p[:-1].split(","):
            if ":" in part:
                cond, val = part.split(":")
                sign = ">"
                if "<" in cond:
                    sign = "<"
                    nam, num = cond.split("<")
                else:
                    nam, num = cond.split(">")
                answers.append((sign, nam, int(num), val))
            else:
                answers.append(part)
        workflows[name] = answers
ans = 0
for inp in inputs:
    st = "in"
    while st != "R" and st != "A":
        for entry in workflows[st]:
            if type(entry) == str:
                st = entry
                break
            elif entry[0] == "<":
                if inp[entry[1]] < entry[2]:
                    st = entry[3]
                    break
            elif entry[0] == ">":
                if inp[entry[1]] > entry[2]:
                    st = entry[3]
                    break
    if st == "A":
        for k, v in inp.items():
            ans += v
print(ans)
