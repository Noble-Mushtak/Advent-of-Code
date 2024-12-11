import sys

stones = []
for line in sys.stdin:
    if line.strip() == "": continue
    stones = list(map(int, line.strip().split()))

def apply_rules(stones):
    new_stones = []
    for stone in stones:
        if stone == 0:
            new_stones.append(1)
        else:
            l = len(str(stone))
            if l % 2 == 0:
                new_stones.append(int(str(stone)[:l//2]))
                new_stones.append(int(str(stone)[l//2:]))
            else:
                new_stones.append(stone*2024)
    return new_stones

for i in range(25):
    stones = apply_rules(stones)

#print(stones)
print(len(stones))
