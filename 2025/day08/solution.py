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

adj=[[] for _ in pts]
        
prs.sort()
for i in range(1000):
    adj[prs[i][1]].append(prs[i][2])
    adj[prs[i][2]].append(prs[i][1])

marked=[False for _ in pts]
szs=[]
for i in range(len(pts)):
    if marked[i]: continue
    q=deque()
    q.append(i)
    marked[i]=True
    sz=1
    while len(q)>0:
        v=q[0]
        q.popleft()
        for y in adj[v]:
            if not marked[y]:
                q.append(y)
                marked[y]=True
                sz+=1
    szs.append(sz)
szs.sort()
print(szs[-3]*szs[-2]*szs[-1])
