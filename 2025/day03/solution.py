import sys

ans = 0
for line in sys.stdin:
    if line == "": continue
    line = line.strip()
    subans = 0
    for i, ch in enumerate(line):
        for ch2 in line[i+1:]:
            subans = max(subans, int(ch+ch2))
    ans += subans
print(ans)
