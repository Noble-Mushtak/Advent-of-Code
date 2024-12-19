import sys
from collections import deque

ans = 0

all_designs = set()
valid_designs = set()

blocks = set()

st = False
for line in sys.stdin:
    if line.strip() == "":
        st = True
        continue
    if st:
        design = line.strip()
        for i in range(1, len(design)+1):
            all_designs.add(design[:i])
        valid_designs.add(design)
    else:
        for tok in line.strip().split(", "):
            blocks.add(tok)

possible_designs = {""}

sorted_designs = list(all_designs)
sorted_designs.sort(key=len)
for x in sorted_designs:
    for y in blocks:
        if x.endswith(y) and x[:len(x)-len(y)] in possible_designs:
            possible_designs.add(x)
            break

for x in valid_designs:
    if x in possible_designs:
        ans += 1

print(ans)
