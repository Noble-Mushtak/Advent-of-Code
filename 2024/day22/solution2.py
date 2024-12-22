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

trueans = 0

mps = [{} for _ in range(len(nums))]

for i, c in enumerate(nums):
    changes = (None, None, None, None)
    sold = 0
    for l in range(2000):
        pastc = c
        c = next_num(c)
        changes = (changes[1], changes[2], changes[3], (c % 10) - (pastc % 10))
        if changes not in mps[i]:
            mps[i][changes] = c % 10
        l += 1
    # print(c, sold)

for x in range(-10, 10):
    for x2 in range(-10, 10):
        for x3 in range(-10, 10):
            print(x, x2, x3)
            for x4 in range(-10, 10):
                ans = 0
                for mp in mps:
                    ans += mp[(x, x2, x3, x4)] if (x,x2,x3,x4) in mp else 0
                trueans = max(trueans, ans)

print(trueans)
