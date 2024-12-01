import sys
l = []
r = []
for line in sys.stdin:
    if line.strip() == "": continue
    a, b = map(int, line.split())
    l.append(a)
    r.append(b)

l.sort()
r.sort()

ans = 0
for i in range(len(l)):
    ans += abs(l[i]-r[i])
print(ans)
