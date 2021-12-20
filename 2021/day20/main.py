import sys

with open(sys.argv[1], "r") as file:
    lines = file.read().strip().split("\n")

def parsech(c):
    return c == "#"
    
boolarr = [parsech(c) for c in lines[0]]
orig_img = []
for i in range(2, len(lines)):
    orig_img.append([parsech(c) for c in lines[i]])

def getch(arr, i, j, def_bool):
    if i < 0: return def_bool
    if j < 0: return def_bool
    try:
        return arr[i][j]
    except IndexError:
        return def_bool

def combinebools(arr):
    num = 0
    for b in arr:
        num <<= 1
        num |= int(b)
    return num

neighbors = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]

margin = 3

def process(img, def_bool):
    height = len(img)
    length = len(img[0])
    new_img = []
    for i in range(-margin, height+margin):
        row = []
        for j in range(-margin, length+margin):
            cells = [getch(img, i+dx, j+dy, def_bool) for (dx, dy) in neighbors]
            row.append(boolarr[combinebools(cells)])
        new_img.append(row)
    return new_img

def getval(img):
    num = 0
    for row in img:
        for b in row:
            num += int(b)
    return num

part1_img = process(process(orig_img, False), True)
print("Part 1:", getval(part1_img))

cur_img = orig_img
alt = False
for i in range(50):
    cur_img = process(cur_img, alt)
    alt = not alt
print("Part 2:", getval(cur_img))
