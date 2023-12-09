import sys
from collections import defaultdict

def nextval(seq):
    all_zeroes = True
    for x in seq:
        if x != 0:
            all_zeroes = False
            break
    if all_zeroes:
        return 0
    assert(len(seq) > 1)
    diffarr = []
    for i in range(len(seq)-1):
        diffarr.append(seq[i+1] - seq[i])
    return seq[-1] + nextval(diffarr)

ans = 0
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    ans += nextval(list(map(int, line.split())))
print(ans)
