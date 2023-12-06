import sys
from collections import defaultdict

a, b = sys.stdin.read().split("\n")
a = list(map(int, a.split()[1:]))
b = list(map(int, b.split()[1:]))

ans = 1
for i in range(len(a)):
    times = []
    for wait in range(a[i]+1):
        times.append(wait * (a[i]-wait))
    mx = max(times)
    subans = 0
    for l in times:
        if l > b[i]:
            subans += 1
    ans *= subans
print(ans)
