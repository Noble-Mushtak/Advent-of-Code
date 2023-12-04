import sys
from collections import defaultdict

idx = 0
ans = 0
cnts = defaultdict(lambda: 0)
for line in sys.stdin:
    if line == "": continue
    
    idx += 1
    cnts[idx] += 1
    ans += cnts[idx]
    
    a, b = line.split(": ")[1].split(" | ")
    a = a.split()
    b = b.split()
    matches = 0
    for n in a:
        if n in b:
            matches += 1
    for i in range(matches):
        cnts[idx + 1 + i] += cnts[idx]

print(ans)
