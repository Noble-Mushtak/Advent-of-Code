import sys

beats = {
    "A": "Y",
    "B": "Z",
    "C": "X"
    }
ties = {
    "A": "X",
    "B": "Y",
    "C": "Z"
    }

ans = 0
for line in sys.stdin:
    other, me = line.split()
    ans += ord(me)-ord("X")+1
    if me == beats[other]:
        ans += 6
    elif me == ties[other]:
        ans += 3
print(ans)
