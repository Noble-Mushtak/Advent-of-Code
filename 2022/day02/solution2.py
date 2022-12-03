import sys

mapping = {
    "A": [3, 1, 2],
    "B": [1, 2, 3],
    "C": [2, 3, 1]
    }

ans = 0
for line in sys.stdin:
    other, res = line.split()
    if res == "Y":
        ans += 3
    elif res == "Z":
        ans += 6
    ans += mapping[other][ord(res)-ord("X")]
print(ans)
