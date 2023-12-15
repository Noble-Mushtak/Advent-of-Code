import sys
from collections import defaultdict, deque

st = None
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    st = line

parts = st.split(",")
boxes = [[] for _ in range(256)]
for part in parts:
    firstp = None
    if "-" in part:
        firstp = part[:-1]
    else:
        firstp = part[:-2]
    subans = 0
    for ch in firstp:
        subans += ord(ch)
        subans *= 17
        subans %= 256
    if "-" in part:
        tr = None
        for i, lens in enumerate(boxes[subans]):
            if lens[0] == firstp:
                tr = i
                break
        if tr != None:
            boxes[subans] = boxes[subans][:tr] + boxes[subans][(tr+1):]
    else:
        tr = None
        for i, lens in enumerate(boxes[subans]):
            if lens[0] == firstp:
                tr = i
                break
        if tr != None:
            boxes[subans][tr] = (firstp, int(part[-1]))
        else:
            boxes[subans].append((firstp, int(part[-1])))
ans = 0
for i in range(256):
    for j, pr in enumerate(boxes[i]):
        ans += (i+1) * (j+1) * pr[1]
print(ans)
