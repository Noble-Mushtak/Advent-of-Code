import sys
from collections import defaultdict, deque
from fractions import Fraction

ST = 200000000000000
ND = 400000000000000
#ST, ND = 7, 27

pts = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    a, b = line.split(" @ ")
    pt1 = tuple(map(int, a.split(", ")))
    pt2 = tuple(map(int, b.split(", ")))
    pts.append((pt1, pt2))
    
ans = 0
for i, pt1 in enumerate(pts):
    for j in range(i+1, len(pts)):
        pt2 = pts[j]
        
        st1, v1 = pt1
        st2, v2 = pt2
        v1o = v1
        v2o = v2
        v1 = Fraction(v1[1], v1[0])
        v2 = Fraction(v2[1], v2[0])
        if v1 == v2:
            assert(st1 != st2)
            continue
        # y = st1[1] + v1 * (x - st1[0])
        #   = st2[1] + v2 * (x - st2[0])
        b1 = st1[1] - v1 * st1[0]
        b2 = st2[1] - v2 * st2[0]
        # v1 * x + b1 = v2 * x + b2 -> (v1-v2)x = b2-b1
        x = (b2-b1) / (v1-v2)
        #print(pt1, pt2, x)
        if v1o[0] > 0:
            if x < st1[0]:
                continue
        else:
            if x > st1[0]:
                continue
            
        if v2o[0] > 0:
            if x < st2[0]:
                continue
        else:
            if x > st2[0]:
                continue
        y = v1 * x + b1
        print(float(x))
        if ST <= x and x <= ND and ST <= y and y <= ND:
            ans += 1

print(ans)
            

