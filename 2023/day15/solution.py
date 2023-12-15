import sys
from collections import defaultdict, deque

st = None
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    st = line

parts = st.split(",")
ans = 0
for part in parts:
    subans = 0
    for ch in part:
        subans += ord(ch)
        subans *= 17
        subans %= 256
    ans += subans
print(ans)
