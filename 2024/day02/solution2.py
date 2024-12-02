import sys
cnt = 0
for line in sys.stdin:
    if line.strip() == "": continue
    nums = list(map(int, line.split()))
    def is_good(arr):
        min_diff = 4
        max_diff = 0
        all_in = True
        all_d = True
        for i in range(len(arr)-1):
            cur_diff = abs(arr[i]-arr[i+1])
            min_diff = min(min_diff, cur_diff)
            max_diff = max(max_diff, cur_diff)
            if not arr[i+1] > arr[i]:
                all_in = False
            if not arr[i+1] < arr[i]:
                all_d = False
        return min_diff >= 1 and max_diff <= 3 and (all_in or all_d)
    bad = False
    for i in range(len(nums)):
        if is_good(nums[:i]+nums[(i+1):]):
            break
    else:
        bad = True
    if not bad: cnt +=1
print(cnt)

        
