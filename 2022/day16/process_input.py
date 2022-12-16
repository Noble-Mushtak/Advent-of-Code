import sys
import json
sys.setrecursionlimit(3000000)

info = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    cur_name = line[6:8]
    rate = int(line.split("=")[1].split(";")[0])
    info.append((cur_name, rate, "".join(line.split(";")[1].split()[4:]).split(",")))

print(len(info))
for a, b, c in info:
    print(a, b, len(c))
    for g in c:
        print(g, end=" ")
    print()
