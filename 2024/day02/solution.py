import sys
cnt = 0
for line in sys.stdin:
    if line.strip() == "": continue
    nums = list(map(int, line.split()))
    min_diff = 4
    max_diff = 0
    all_in = True
    all_d = True
    for i in range(len(nums)-1):
        cur_diff = abs(nums[i]-nums[i+1])
        min_diff = min(min_diff, cur_diff)
        max_diff = max(max_diff, cur_diff)
        if not nums[i+1] > nums[i]:
            all_in = False
        if not nums[i+1] < nums[i]:
            all_d = False
    if min_diff >= 1 and max_diff <= 3 and (all_in or all_d):
        cnt += 1
print(cnt)
        
