import copy
import sys
from collections import deque

ans = 0

nums = []

for line in sys.stdin:
    if line.strip() == "":
        continue
    nums.append(int(line.strip()))

def next_num(x):
    x = x ^ (x << 6)
    x = x & ((1 << 24)-1)
    x = x ^ (x >> 5)
    x = x & ((1 << 24)-1)
    x = x ^ (x << 11)
    x = x & ((1 << 24)-1)
    return x

mps = [{} for _ in range(len(nums))]

for c in nums:
    for i in range(2000):
        c = next_num(c)
    ans += c

print(ans)
