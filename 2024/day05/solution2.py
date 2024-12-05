import sys
ans = 0
state = False
rules = []
for line in sys.stdin:
    if line.strip() == "":
        if not state:
            state = True
        continue
    if not state:
        a,b = map(int,line.split("|"))
        rules.append((a,b))
    else:
        stocks = list(map(int,line.split(",")))
        bad = False
        for i in range(len(stocks)):
            for j in range(i):
                if (stocks[i], stocks[j]) in rules:
                    bad = True
                    break
        if bad:
            while bad:
                bad = False
                for i in range(len(stocks)):
                    for j in range(i):
                        if (stocks[i], stocks[j]) in rules:
                            stocks[i], stocks[j] = stocks[j], stocks[i]
                            bad = True
                            break
                    if bad:
                        break
            ans += stocks[len(stocks)//2]
                        
print(ans)
