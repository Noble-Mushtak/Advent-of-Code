import sys

BLOCK = "\u2588"

changes = []
for line in sys.stdin:
    changes.append(0)
    if line[:4] == "addx":
        changes.append(int(line[5:]))

val = 1
for i, dx in enumerate(changes):
    if (i % 40) == val-1 or (i % 40) == val or (i % 40) == val+1:
        print(BLOCK, end="")
    else:
        print(" ", end="")
    if i % 40 == 39:
        print("")
    val += dx
