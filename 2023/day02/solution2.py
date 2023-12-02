import sys

ans = 0
for line in sys.stdin:
    if line == "": continue
    part1, part2 = line.strip().split(": ")
    part1 = part1.split(" ")[1]
    limit = {"red": 0, "green": 0, "blue": 0}
    bad = False
    for partp in part2.split("; "):
        for portion in partp.split(", "):
            n, s = portion.split(" ")
            limit[s] = max(limit[s], int(n))
    print(limit)
    prod = 1
    for k, v in limit.items():
        prod *= v
    ans += prod
print(ans)
