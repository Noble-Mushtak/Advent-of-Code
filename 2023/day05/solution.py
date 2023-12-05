import sys
from collections import defaultdict

initial_seeds = []
domains = defaultdict(list)
cur_key = None
keyorder = ["seed"]
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    if line.startswith("seeds:"):
        initial_seeds = list(map(int, line.split(": ")[1].split()))
        print("I", initial_seeds)
    elif line[-1] == ":":
        a, b = line.split()[0].split("-to-")
        cur_key = a, b
        assert(keyorder[-1] == a)
        keyorder.append(b)
    else:
        a, b, c = line.split()
        domains[cur_key].append((int(a), int(b), int(c)))

ans = 10000000000000
for s in initial_seeds:
    cur_val = s
    last_key = "seed"
    for k in keyorder[1:]:
        new_val = cur_val
        for dst_st, src_st, rng_sz in domains[(last_key, k)]:
            if src_st <= cur_val and cur_val < src_st + rng_sz:
                new_val = cur_val - src_st + dst_st
                break
        cur_val = new_val
        last_key = k
        print(s, k, cur_val)
    ans = min(ans, cur_val)
print(ans)
