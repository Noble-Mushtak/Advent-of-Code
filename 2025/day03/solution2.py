import sys

def best_tpl(t1, t2):
    x1,y1 = t1
    x2,y2 = t2
    if x1 > x2:
        return t1
    if x1 < x2:
        return t2
    if y1 < y2:
        return t1
    return t2

ans = 0
for line in sys.stdin:
    if line == "": continue
    line = line.strip()
    lst = []
    for i in range(12):
        subans = (0, len(line))
        for j, ch in enumerate(line[:i-11] if i < 11 else line):
            subans = best_tpl(subans, (int(ch), j))
        lst.append(str(subans[0]))
        line = line[subans[1]+1:]
    ans += int("".join(lst))
print(ans)
