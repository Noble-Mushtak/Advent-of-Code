import sys

limit = {"red": 12, "green": 13, "blue": 14}

ans = 0
for line in sys.stdin:
    if line == "": continue
    part1, part2 = line.strip().split(": ")
    part1 = part1.split(" ")[1]
    bad = False
    for partp in part2.split("; "):
        for portion in partp.split(", "):
            n, s = portion.split(" ")
            if int(n) > limit[s]:
                bad = True
                break
    if not bad:
        print(part1)
        ans += int(part1)
print(ans)
