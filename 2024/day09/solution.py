import sys
from collections import deque

inpt = None
for line in sys.stdin:
    if line.strip() == "": continue
    inpt = line.strip()

array = []
place = True
curid = 0
first_none = None
for ch in inpt:
    x = int(ch)
    for i in range(x):
        if place:
            array.append(curid)
        else:
            if first_none is None:
                first_none = len(array)
            array.append(None)
    if place:
        curid += 1
    place = not place

while first_none < len(array):
    while array[-1] == None:
        array.pop()
    if first_none >= len(array):
        break
    array[first_none] = array[-1]
    array.pop()
    while first_none < len(array) and array[first_none] != None:
        first_none += 1

ans = 0
for i in range(len(array)):
    ans += i*array[i]

print(ans)
