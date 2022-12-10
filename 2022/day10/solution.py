import sys

changes = []
for line in sys.stdin:
    changes.append(0)
    if line[:4] == "addx":
        changes.append(int(line[5:]))

ans = 0
val = 1
for i, dx in enumerate(changes):
    if i % 40 == 19:
        ans += (i+1)*val
    val += dx
print(ans)
