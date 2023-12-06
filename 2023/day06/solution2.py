import sys
from collections import defaultdict

a, b = sys.stdin.read().split("\n")
a = int("".join(a.split()[1:]))
b = int("".join(b.split()[1:]))
print(a, b)

def can_beat(wait):
    return wait * (a-wait) > b

mn = 0
while not can_beat(mn):
    mn += 10

while can_beat(mn-1):
    mn -= 1

mx = a
while not can_beat(mx):
    mx -= 10

while can_beat(mx+1):
    mx += 1

print(mn, mx)
print(mx - mn + 1)
