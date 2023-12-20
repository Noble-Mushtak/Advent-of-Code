import sys
from collections import defaultdict, deque

sts = []
modules = {}
inps = defaultdict(list)
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    name, parts = line.split(" -> ")
    parts = parts.split(", ")
    if name == "broadcaster":
        sts = parts
    elif name == "output":
        pass
    else:
        modules[name[1:]] = (name[0], parts)
        for p in parts:
            inps[p].append(name[1:])

num_los = 0
num_his = 0
cur_status = defaultdict(lambda: False)
cur_inputs = {}

for name in modules:
    if modules[name][0] == "&":
        cur_inputs[name] = {}
        for inp in inps[name]:
            cur_inputs[name][inp] = False

for i in range(1000):
    queue = deque()
    num_los += 1
    for st in sts:
        queue.append(("broadcaster", False, st))
    while len(queue) > 0:
        inp_st, cur_sig, cur_st = queue.popleft()
        #print(i, inp_st, cur_sig, cur_st)
        if cur_sig:
            num_his += 1
        else:
            num_los += 1
        if cur_st == "output":
            continue
        if cur_st not in modules:
            continue
        if modules[cur_st][0] == "%":
            if not cur_sig:
                cur_status[cur_st] = not cur_status[cur_st]
                new_sig = cur_status[cur_st]
                for new_st in modules[cur_st][1]:
                    queue.append((cur_st, new_sig, new_st))
        else:
            assert(modules[cur_st][0] == "&")
            cur_inputs[cur_st][inp_st] = cur_sig
            lo_found = False
            for k, v in cur_inputs[cur_st].items():
                if not v:
                    lo_found = True
                    break
            new_sig = lo_found
            for new_st in modules[cur_st][1]:
                queue.append((cur_st, new_sig, new_st))
    #print(i, num_los, num_his)


print(num_los, num_his)
print(num_los*num_his)
