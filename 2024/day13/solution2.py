import sys
from fractions import Fraction

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
        x = int(toks[1][2:-1])+10000000000000
        y = int(toks[2][2:])+10000000000000

        a, b, c, d = cura[0], curb[0], cura[1], curb[1]
        if a*d-b*c != 0:
            i = Fraction(d, a*d-b*c) * x + Fraction(-b, a*d-b*c) * y
            j = Fraction(-c, a*d-b*c) * x + Fraction(a, a*d-b*c) * y
            if i > 0 and j > 0 and i == int(i) and j == int(j):
                assert a*i + b*j == x
                assert c*i + d*j == y
                ans += 3*i+j

print(ans)
