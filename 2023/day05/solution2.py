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
        domains[cur_key].append((int(b), int(a), int(c)))

for x in domains:
    domains[x].sort()
    domains[x].append((10000000000000000, 10000000000000000, 1))

ans = 10000000000000
for idx in range(0, len(initial_seeds), 2):
    ranges = [(initial_seeds[idx], initial_seeds[idx] + initial_seeds[idx + 1] - 1)]
    last_key = "seed"
    for k in keyorder[1:]:
        new_ranges = []
        for st, nd in ranges:
            last_nd = -1000000000000000000000
            orig_st, orig_nd = st, nd
            for src_st, dst_st, rng_sz in domains[(last_key, k)]:
                if st > last_nd and st < src_st:
                    new_ranges.append((st, min(nd, src_st-1)))
                    st = src_st
                    if st > nd:
                        break
                if st > src_st+rng_sz-1:
                    continue
                print(st, nd, src_st, dst_st, rng_sz)
                new_ranges.append((st - src_st + dst_st, min(src_st+rng_sz-1, nd) - src_st + dst_st))
                assert(new_ranges[-1][0] <= new_ranges[-1][1])
                st = src_st+rng_sz
                if st > nd:
                    break
                last_nd = src_st+rng_sz-1
            print(k, orig_st, orig_nd, new_ranges)
        last_key = k
        ranges = new_ranges
        print(k, ranges)
    for a, b in ranges:
        ans = min(ans, a)
print(ans)
