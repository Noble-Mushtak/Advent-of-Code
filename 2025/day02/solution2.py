import sys

inpt = sys.stdin.read().strip()
prs = [tuple(map(int,x.split("-"))) for x in inpt.split(",")]
prs.sort()
for i, pr in enumerate(prs[1:]):
    assert prs[i][1] < pr[0], (prs[i], pr)

def double_geq(st):
    st_str = str(st)
    half_len = len(st_str)//2
    if len(st_str) % 2 == 1:
        return 10**half_len

    half_st = int(st_str[:half_len])
    candidate = half_st*10**half_len + half_st
    if candidate >= st:
        return half_st
    else:
        return half_st+1

ans = 0
for st, nd in prs:
    last_num_out = double_geq(st)-1
    last_num_out_len = len(str(last_num_out))
    last_num_in = double_geq(nd+1)-1
    last_num_in_len = len(str(last_num_in))
    for l in range(last_num_out_len, last_num_in_len+1):
        first_term = last_num_out+1 if l == last_num_out_len else 10**(l-1)
        last_term = last_num_in if l == last_num_in_len else 10**l-1
        sum = ((last_term + first_term) * (last_term - first_term + 1)) // 2
        ans += sum*(10**l+1)

for rep in [3, 5, 7, 11]:
    to_rep = 1
    while True:
        to_rep_str = str(to_rep)
        repd = int("".join((to_rep_str for _ in range(rep))))
        if repd > prs[-1][1]:
            break
        repd_len = rep*len(to_rep_str)
        if repd_len % 2 == 0:
            half_len = repd_len//2
            repd_str = str(repd)
            if repd_str[:half_len] == repd_str[half_len:]:
                to_rep += 1
                continue
        for st, nd in prs:
            if st <= repd and repd <= nd:
                ans += repd
                break
        to_rep += 1

print(ans)
