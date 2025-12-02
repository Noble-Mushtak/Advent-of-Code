import sys

location = 50
ans = 0

for line in sys.stdin:
    line = line.strip()
    if len(line) == 0: continue
    pos = int(line[1:])

    ans += pos // 100
    pos %= 100
    
    if line[0] == "L":
        ans += int(location <= pos and location != 0)
        pos *= -1
    else:
        ans += int(location+pos >= 100)
    location = (location + pos) % 100

print(ans)
