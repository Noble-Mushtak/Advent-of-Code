import sys

ans = 0

inputs = []
cells = []
cur_input = []

st = 0
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    if st == 0:
        if line.endswith(":"):
            st = 1
        else:
            toks = line.split()
            rest = toks[1:]
            start = toks[0][:-1]
            tok1, tok2 = start.split("x")
            tok1 = int(tok1)
            tok2 = int(tok2)
            rest = list(map(int, rest))
            if sum(rest) <= (tok1//3)*(tok2//3):
                ans += 1
            else:
                needed_cells = 0
                for i, rst in enumerate(rest):
                    needed_cells += cells[i] * rst
                assert needed_cells > tok1*tok2
    else:
        cur_input.append(line)
        if st == 3:
            cur_cells = 0
            for line2 in cur_input:
                cur_cells += len(line2.split("#"))-1
            inputs.append(cur_input)
            cells.append(cur_cells)
            cur_input = []
            st = 0
        else:
            st += 1

print(ans)
