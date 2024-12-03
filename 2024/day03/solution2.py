import sys
ans = 0
skip = False
for line in sys.stdin:
    if line.strip() == "": continue
    for i in range(len(line)-3):
        if line[i:i+len("don't()")] == "don't()":
            skip = True
            print(i, ans, skip, line[i:i+10])
            continue
        if line[i:i+len("do()")] == "do()":
            skip = False
            print(i, ans, skip, line[i:i+10])
            continue
            
        if not skip and line[i:i+4] == "mul(":
            digs = 0
            while i+4+digs < len(line) and ord(line[i+4+digs]) >= ord("0") and ord(line[i+4+digs]) <= ord("9"):
                digs += 1
            if digs == 0:
                continue
            if line[i+4+digs] != ",":
                continue
            digs2 = 0
            while i+4+digs+1+digs2 < len(line) and ord(line[i+4+digs+1+digs2]) >= ord("0") and ord(line[i+4+digs+1+digs2]) <= ord("9"):
                digs2 += 1
            if digs2 == 0:
                continue
            if line[i+4+digs+1+digs2] != ")":
                continue
            ans += int(line[i+4:i+4+digs]) * int(line[i+4+digs+1:i+4+digs+1+digs2])
print(ans)
