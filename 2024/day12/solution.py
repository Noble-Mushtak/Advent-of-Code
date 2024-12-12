import sys
from collections import defaultdict

# https://github.com/shakayami/ACL-for-python/blob/master/dsu.py
class dsu():
    n=1
    parent_or_size=[-1 for i in range(n)]
    def __init__(self,N):
        self.n=N
        self.parent_or_size=[-1 for i in range(N)]
    def merge(self,a,b):
        assert 0<=a<self.n, "0<=a<n,a={0},n={1}".format(a,self.n)
        assert 0<=b<self.n, "0<=b<n,b={0},n={1}".format(b,self.n)
        x=self.leader(a)
        y=self.leader(b)
        if x==y:
            return x
        if (-self.parent_or_size[x]<-self.parent_or_size[y]):
            x,y=y,x
        self.parent_or_size[x]+=self.parent_or_size[y]
        self.parent_or_size[y]=x
        return x
    def same(self,a,b):
        assert 0<=a<self.n, "0<=a<n,a={0},n={1}".format(a,self.n)
        assert 0<=b<self.n, "0<=b<n,b={0},n={1}".format(b,self.n)
        return self.leader(a)==self.leader(b)
    def leader(self,a):
        assert 0<=a<self.n, "0<=a<n,a={0},n={1}".format(a,self.n)
        if (self.parent_or_size[a]<0):
            return a
        self.parent_or_size[a]=self.leader(self.parent_or_size[a])
        return self.parent_or_size[a]
    def size(self,a):
        assert 0<=a<self.n, "0<=a<n,a={0},n={1}".format(a,self.n)
        return -self.parent_or_size[self.leader(a)]
    def groups(self):
        leader_buf=[0 for i in range(self.n)]
        group_size=[0 for i in range(self.n)]
        for i in range(self.n):
            leader_buf[i]=self.leader(i)
            group_size[leader_buf[i]]+=1
        result=[[] for i in range(self.n)]
        for i in range(self.n):
            result[leader_buf[i]].append(i)
        result2=[]
        for i in range(self.n):
            if len(result[i])>0:
                result2.append(result[i])
        return result2

grid = []

for line in sys.stdin:
    if line.strip() == "": continue
    grid.append(line.strip())

N = len(grid)
uf = dsu(N*N)

def to_coord(a,b):
    return N*a+b
def to_pt(y):
    return (y//N,y%N)

dirs = [(-1,0),(1,0),(0,-1),(0,1)]
for i in range(N):
    for j in range(N):
        for dx, dy in dirs:
            newx, newy = i+dx, j+dy
            if newx >= 0 and newx < N and newy >= 0 and newy < N and grid[i][j] == grid[newx][newy]:
                uf.merge(to_coord(i,j),to_coord(newx,newy))

comps = uf.groups()
comp_map = {}
for x in comps:
    print(x)
    for y in x:
        comp_map[y] = x[0]

bnds = defaultdict(lambda: 0)
for i in range(N+1):
    for j in range(N+1):
        curcomps = defaultdict(lambda: set())
        bad = False
        for dx, dy in [(0,0),(-1,-1),(-1,0),(0,-1)]:
            newx, newy = i+dx, j+dy
            if newx >= 0 and newx < N and newy >= 0 and newy < N:
                curcomps[comp_map[to_coord(newx,newy)]].add((dx,dy))
            else:
                bad = True
        if len(curcomps) != 1 or bad:
            for c, s in curcomps.items():
                if s == {(-1,-1),(0,0)} or s == {(-1,0),(0,-1)}:
                    bnds[c] += 2
                else:
                    bnds[c] += 1
                
ans = 0
for c, B in bnds.items():
    truth = 0
    for x in comps:
        if c in x:
            truth = len(x)
    ans += B * truth
                
print(ans)
