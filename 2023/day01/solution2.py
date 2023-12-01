import sys

strs = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tot = 0
for line in sys.stdin:
    if line == "": continue
    ans = []
    idx = 0
    while idx < len(line):
        for i in range(len(strs)):
            s = strs[i]
            if line[idx:].startswith(s):
                idx += 1
                ans.append(i % 10)
                break
        else:
            idx += 1
    # print(line, ans)
    tot += 10*ans[0]+ans[-1]
print(tot)
