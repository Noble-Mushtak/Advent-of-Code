import sys

tot = 0
for line in sys.stdin:
    if line == "": continue
    ans = ""
    for c in line:
        if ord(c) >= ord("0") and ord(c) <= ord("9"):
            ans += c
    tot += int(ans[0]+ans[-1])
print(tot)
