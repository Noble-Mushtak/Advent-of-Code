import sys
from collections import defaultdict

grid=[]
for line in sys.stdin:
    line=line.strip()
    if line=="": continue
    grid.append(line)
streams = defaultdict(lambda: 0)
for line in grid:
    ns = defaultdict(lambda: 0)
    for i,ch in enumerate(line):
        if ch=="S":
            ns[i]+=1
            continue
        if i in streams:
            if ch == "^":
                ns[i-1]+=streams[i]
                ns[i+1]+=streams[i]
                assert i-1>=0
                assert i+1<len(line)
            else:
                ns[i]+=streams[i]
    streams=ns
ans=0
for k,v in streams.items(): ans += v
print(ans)
