import sys
from collections import defaultdict, deque
from z3 import *

pts = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    a, b = line.split(" @ ")
    pt1 = tuple(map(int, a.split(", ")))
    pt2 = tuple(map(int, b.split(", ")))
    pts.append((pt1, pt2))
    
# first six variables -> rx, ry, rz, vx, vy, vz
rx = Int("rx")
ry = Int("ry")
rz = Int("rz")
vx = Int("vx")
vy = Int("vy")
vz = Int("vz")

formulas = []

for i, pt in enumerate(pts):
    st, v = pt
    time = Int(f"t{i}")
    
    formulas.append(rx + time * vx == st[0] + time * v[0])
    formulas.append(ry + time * vy == st[1] + time * v[1])
    formulas.append(rz + time * vz == st[2] + time * v[2])
    formulas.append(time >= 0)
    formulas.append(rx < 2**128)
    formulas.append(ry < 2**128)
    formulas.append(rz < 2**128)

s = Solver()
s.add(*formulas)
print(s.check())
model = s.model()
print(model.evaluate(rx) + model.evaluate(ry) + model.evaluate(rz))
