import sys
from collections import defaultdict, deque
ans = 0
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    grid, nums = line.split()
    nums = list(map(int, nums.split(",")))
    
    grid = grid+"?"+grid+"?"+grid+"?"+grid+"?"+grid
    nums = nums+nums+nums+nums+nums
    
    arrangements = 1
    sts = defaultdict(int)
    sts[(0, 0)] += 1
    for i in range(len(grid)):
        new_sts = defaultdict(int)
        poss_chars = [grid[i]]
        if grid[i] == "?":
            poss_chars = [".", "#"]
        for k, v in sts.items():
            sofar, idx = k
            for ch in poss_chars:
                if idx == len(nums):
                    if ch == ".":
                        new_sts[(sofar, idx)] += v
                else:
                    if sofar == nums[idx]:
                        if ch == ".":
                            new_sts[(0, idx+1)] += v
                    else:
                        if ch == "." and sofar == 0:
                            new_sts[(sofar, idx)] += v
                        if ch == "#":
                            new_sts[(sofar+1, idx)] += v
        sts = new_sts
    # print(sts[(0, len(nums))])
    ans += sts[(0, len(nums))] + sts[(nums[-1], len(nums)-1)]

print(ans)
