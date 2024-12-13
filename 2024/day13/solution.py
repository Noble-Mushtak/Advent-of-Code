import sys

ans = 0

cura = None
curb = None

st = 0
for line in sys.stdin:
    if line.strip() == "":
        st = 0
        continue
    if st == 0:
        toks = line.split()
        x = int(toks[2][2:-1])
        y = int(toks[3][2:])
        cura = (x,y)
        st = 1
    elif st == 1:
        toks = line.split()
        x = int(toks[2][2:-1])
        y = int(toks[3][2:])
        curb = (x,y)
        st = 2
    else:
        toks = line.split()
        x = int(toks[1][2:-1])
        y = int(toks[2][2:])

        score = 1000000
        for i in range(101):
            for j in range(101):
                if i*cura[0] + j*curb[0] == x and i*cura[1] + j*curb[1] == y:
                    score = min(score, 3*i+j)
                if i == 80 and j == 40:
                    print(i*cura[0] + j*curb[0], x, cura[1], curb[1],i*cura[1] + j*curb[1], y)
        if score < 1000000:
            ans += score
        st = 0

print(ans)
