import sys

grid=[]
ans=0
for line in sys.stdin:
    line=line.strip()
    if line=="": continue
    grid.append(line)
streams = set()
for line in grid:
    ns = set()
    for i,ch in enumerate(line):
        if ch=="S":
            ns.add(i)
            continue
        if i in streams:
            if ch == "^":
                ns.add(i-1)
                ns.add(i+1)
                assert i-1>=0
                assert i+1<len(line)
                ans+=1
            else:
                ns.add(i)
    streams=ns
print(ans)
