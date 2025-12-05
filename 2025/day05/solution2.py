import sys
import bisect
ranges =[]
sorted=False
ans=0
for line in sys.stdin:
    line=line.strip()
    if line=="": continue
    if "-" in line:
        ranges.append(tuple(map(int,line.split("-"))))
ranges.sort()
sorted=True

new_ranges=[]
cur=ranges[0]
for i in range(len(ranges)-1):
    if cur[1]<ranges[i+1][0]:
        new_ranges.append(cur)
        cur=ranges[i+1]
    else:
        cur=(cur[0],max(cur[1],ranges[i+1][1]))
new_ranges.append(cur)
ranges=new_ranges
for x,y in ranges:
    ans+=y-x+1
print(ans)
