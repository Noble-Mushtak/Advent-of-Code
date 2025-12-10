import sys
from collections import deque

def tag_to_num(tag):
    num = 0
    for ch in reversed(tag):
        num *= 2
        if ch == "#":
            num += 1
    return num

def bfs_on_tools(exp, tools):
    vis = set()
    bfs_q = deque()
    bfs_q.append((0,0))
    vis.add(0)
    while True:
        assert len(bfs_q) > 0
        (v, c) = bfs_q[0]
        bfs_q.popleft()
        if v == exp:
            return c
        for tool in tools:
            if v ^ tool not in vis:
                bfs_q.append((v ^ tool, c+1))
                vis.add(v ^ tool)

ans = 0

for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    toks = line.split()
    exp = tag_to_num(toks[0][1:-1])

    tools = []
    for tok in toks[1:-1]:
        num = 0
        for numtok in tok[1:-1].split(","):
            num += 2**int(numtok)
        tools.append(num)

    subans = bfs_on_tools(exp, tools)
    print(subans)
    ans += subans
print(ans)
