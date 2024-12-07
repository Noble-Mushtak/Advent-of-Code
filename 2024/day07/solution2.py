import sys

ans = 0

for line in sys.stdin:
    if line.strip() == "":
        continue
    toks = list(line.split())
    val = int(toks[0][:-1])
    vals = list(map(int,toks[1:]))
    good = False
    for i in range(3**(len(vals)-1)):
        anss = vals[0]
        for j in range(len(vals)-1):
            tv = (i//3**j) % 3
            if tv == 2:
                anss = int(str(anss)+str(vals[j+1]))
            elif tv == 1:
                anss *= vals[j+1]
            else:
                anss += vals[j+1]
        if anss == val:
            good = True
            break
    if good:
        ans += val
        print("EHY", val)
print(ans)
