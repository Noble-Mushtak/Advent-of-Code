import sys

W = 101
H = 103

ans = [0,0,0,0]

st = 0
for line in sys.stdin:
    if line.strip() == "":
        continue
    p,v = line.split()
    px,py = map(int,p[2:].split(",")) 
    vx,vy = map(int,v[2:].split(","))
    nx,ny = px + 100*vx, py + 100*vy
    nx = nx % W
    ny = ny % H
    if nx == W//2 or ny == H//2:
        continue
    if nx < W//2 and ny < H//2:
        ans[0] += 1
    elif nx > W//2 and ny < H//2:
        ans[1] += 1
    elif nx < W//2 and ny > H//2:
        ans[2] += 1
    else:
        ans[3] += 1
        
print(ans)
print(ans[0] * ans[1] * ans[2] * ans[3])
