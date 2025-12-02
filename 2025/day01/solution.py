import sys

location = 50
ans = 0

for line in sys.stdin:
    line = line.strip()
    if len(line) == 0: continue
    pos = int(line[1:])
    if line[0] == "L":
        pos *= -1
    location = (location + pos) % 100
    ans += int(location == 0)

print(ans)
