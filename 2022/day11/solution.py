import sys

monkeys = []
ops = []
divs = []
edges = []
for info in sys.stdin.read().split("\n\n"):
    lines = info.split("\n")
    items = list(map(int, lines[1][18:].split(", ")))
    op_str = lines[2][13:]
    if op_str.startswith("new = old + "):
        other = int(op_str[len("new = old + "):])
        ops.append(["add", other])
    elif op_str.startswith("new = old * "):
        other = op_str[len("new = old * "):]
        if other == "old":
            ops.append(["square"])
        else:
            other = int(other)
            ops.append(["mult", other])
    div = int(lines[3][len("  Test: divisible by "):])
    n1 = int(lines[4][len("    If true: throw to monkey "):])
    n2 = int(lines[5][len("    If false: throw to monkey "):])

    monkeys.append(items)
    divs.append(div)
    edges.append((n1, n2))

def apply_op(op, num):
    if op[0] == "square":
        return num*num
    if op[0] == "add":
        return num+op[1]
    return num*op[1]

cnts = [0 for _ in range(len(monkeys))]
for _ in range(20):
    for i in range(len(monkeys)):
        for item in monkeys[i]:
            cnts[i] += 1
            new_item = apply_op(ops[i], item)//3
            if new_item % divs[i] == 0:
                monkeys[edges[i][0]].append(new_item)
            else:
                monkeys[edges[i][1]].append(new_item)
        monkeys[i] = []

cnts.sort()
print(cnts[-1]*cnts[-2])
