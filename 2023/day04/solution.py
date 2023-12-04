import sys

sum = 0
for line in sys.stdin:
    if line == "": continue
    a, b = line.split(": ")[1].split(" | ")
    a = a.split()
    b = b.split()
    pts = 0
    for n in a:
        if n in b:
            if pts == 0:
                pts = 1
            else:
                pts *= 2
    sum += pts

print(sum)
