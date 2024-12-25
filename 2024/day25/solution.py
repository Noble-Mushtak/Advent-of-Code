import copy
import sys
from collections import deque, defaultdict
from functools import cache

ans = 0
locks = []
keys = []
buffer = []

SPACES = 5

def process_buffer(buffer):
    if len(buffer) > 0:
        if buffer[0] == "#####":
            heights = []
            for j in range(len(buffer[0])):
                height = len(buffer)
                for i in range(len(buffer)):
                    if buffer[i][j] == ".":
                        height = i-1
                        break
                heights.append(height)
            locks.append(tuple(heights))
        else:
            assert buffer[-1] == "#####"
            heights = []
            for j in range(len(buffer[0])):
                height = len(buffer)
                for i in range(len(buffer)):
                    if buffer[len(buffer)-1-i][j] == ".":
                        height = i-1
                        break
                heights.append(height)
            keys.append(tuple(heights))

for line in sys.stdin:
    if line.strip() == "":
        process_buffer(buffer)
        buffer = []
        continue
    buffer.append(line.strip())
process_buffer(buffer)

for key in keys:
    for lock in locks:
        #print(key, lock)
        good = True
        for i in range(len(key)):
            if key[i] + lock[i] > SPACES:
                good = False
                break
        if good:
            ans += 1

print(ans)
