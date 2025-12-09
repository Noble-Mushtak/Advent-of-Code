import sys

points = []
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    points.append(tuple(map(int, line.split(","))))

area = 0
for i, pt in enumerate(points):
    for j, pt2 in enumerate(points[:i]):
        x1,y1=pt
        x2,y2=pt2
        area = max(area, (max(y1,y2)-min(y1,y2)+1)*(max(x1,x2)-min(x1,x2)+1))
print(area)
    
