import sys
from collections import defaultdict

stones = defaultdict(lambda: 0)
for line in sys.stdin:
    if line.strip() == "": continue
    for x in map(int, line.strip().split()):
        stones[x] += 1

    
def apply_rules(stones):
    new_stones = defaultdict(lambda: 0)
    for stone, freq in stones.items():
        if stone == 0:
            new_stones[1] += freq
        else:
            l = len(str(stone))
            if l % 2 == 0:
                new_stones[int(str(stone)[:l//2])] += freq
                new_stones[int(str(stone)[l//2:])] += freq
            else:
                new_stones[stone*2024] += freq
    return new_stones

for i in range(75):
    stones = apply_rules(stones)

#print(stones)
ans = 0
for _, freq in stones.items():
    ans += freq
print(ans)
