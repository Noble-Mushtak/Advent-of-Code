from collections import deque
import sys
pts=[]
for line in sys.stdin:
    line=line.strip()
    if line=="": continue
    pts.append(list(map(int,line.split(","))))
prs=[]
for i in range(len(pts)):
    for j in range(i):
        dst = 0
        for z in range(3):
            dst+=(pts[i][z]-pts[j][z])**2
        prs.append((dst,i,j))

par=list(range(len(pts)))
sz=[1 for _ in pts]
def gp(x):
    if par[x]==x: return x
    s=gp(par[x])
    par[x]=s
    return s

prs.sort()
for _,i,j in prs:
    p1,p2=gp(i),gp(j)
    if p1!=p2:
        if sz[p1]>sz[p2]:
            p1,p2=p2,p1
        par[p1]=p2
        sz[p2]+=sz[p1]
        if sz[p2]==len(pts):
            print(pts[i][0]*pts[j][0])
            break
