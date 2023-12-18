import sys
from collections import defaultdict, deque
import numpy as np

# https://stackoverflow.com/a/30408825
def PolyArea(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

loc = (0, 0)
num_pts = 0
xs = []
ys = []
dir_map = [(0, 1), (-1, 0), (0, -1), (1, 0)]
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    a, b, c = line.split()
    
    cur_dir = dir_map[int(c[-2])]
    cur_len = int(c[-7:-2], 16)
    
    num_pts += cur_len
    loc = (loc[0]+cur_len*cur_dir[0], loc[1]+cur_len*cur_dir[1])
    xs.append(loc[0])
    ys.append(loc[1])
    print(loc)

A = PolyArea(xs, ys)
b = num_pts
# A = i + b/2 - 1 -> i = A + 1 - b/2
assert(b % 2 == 0)
I = A + 1 - b // 2
print(I+b)
