import sys

cur_dir = []

files = {}

def add_dir(cur_dir, name):
    cur_ref = files
    for seg in cur_dir:
        if seg not in cur_ref:
            cur_ref[seg] = {}
        cur_ref = cur_ref[seg]
    cur_ref[name] = {}

def add_file(cur_dir, name, size):
    cur_ref = files
    for seg in cur_dir:
        if seg not in cur_ref:
            cur_ref[seg] = {}
        cur_ref = cur_ref[seg]
    cur_ref[name] = size

for line in sys.stdin:
    if line[0] == "$":
        if line.startswith("$ ls"):
            pass
        elif line.startswith("$ cd"):
            cur_seg = line[5:].strip()
            if cur_seg == "/":
                cur_dir = []
            elif cur_seg == "..":
                cur_dir.pop()
            else:
                cur_dir.append(cur_seg)
    else:
        n, name = line.split()
        if n == "dir":
            add_dir(cur_dir, name)
        else:
            add_file(cur_dir, name, int(n))
            
def sum_over_thresh(cur_ref):
    ans = 0
    sze = 0
    for name, it in cur_ref.items():
        if type(it) == int:
            sze += it
        else:
            cur_ans, cur_sze = sum_over_thresh(it)
            ans += cur_ans
            sze += cur_sze
    if sze <= 100000:
        ans += sze
    return ans, sze

ans1, total_sze = sum_over_thresh(files)
print(ans1)

sze_needed = total_sze-40000000
def at_least(cur_ref):
    ans = float("inf")
    sze = 0
    for name, it in cur_ref.items():
        if type(it) == int:
            sze += it
        else:
            cur_ans, cur_sze = at_least(it)
            ans = min(ans, cur_ans)
            sze += cur_sze
    if sze >= sze_needed:
        ans = min(ans, sze)
    return ans, sze
    
print(at_least(files)[0])
