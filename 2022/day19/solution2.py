import sys
import json
sys.setrecursionlimit(3000000)

MAXT = 32

ans = 1
for line in sys.stdin:
    line = line.strip()
    if line == "": continue
    words = line.split()
    num = int(words[1][:-1])
    ore_rob = int(words[6])
    
    clay_rob = int(words[12])
    
    ob_ore = int(words[18])
    ob_clay = int(words[21])

    ge_ore = int(words[27])
    ge_ob = int(words[30])

    states = [(0, 0, 0, 0, 1, 0, 0, 0)]
    max_ge = 0
    for t in range(MAXT):
        new_states = set()
        max_ge = 0
        max_poss_ge = 0
        for state in states:
            new_ore = state[0]+state[4]
            new_clay = state[1]+state[5]
            if state[6] >= ge_ob:
                new_clay = 0
            new_ob = state[2]+state[6]
            new_ge = state[3]+state[7]
            max_ge = max(new_ge, max_ge)
            max_poss_ge = max(new_ge+state[7]*(MAXT-1-t), max_poss_ge)
            new_states.add((new_ore, new_clay, new_ob, new_ge, state[4], state[5], state[6], state[7]))
            if state[0] >= ore_rob and state[4] < max(clay_rob, ob_ore, ge_ore):
                new_states.add((new_ore-ore_rob, new_clay, new_ob, new_ge, state[4]+1, state[5], state[6], state[7]))
            if state[0] >= clay_rob and state[5] < ob_clay:
                new_states.add((new_ore-clay_rob, new_clay, new_ob, new_ge, state[4], state[5]+1, state[6], state[7]))
            if state[0] >= ob_ore and state[1] >= ob_clay and state[6] < ge_ob:
                new_states.add((new_ore-ob_ore, new_clay-ob_clay, new_ob, new_ge, state[4], state[5], state[6]+1, state[7]))
            if state[0] >= ge_ore and state[2] >= ge_ob:
                new_states.add((new_ore-ge_ore, new_clay, new_ob-ge_ob, new_ge, state[4], state[5], state[6], state[7]+1))
        states = set()
        if t < MAXT-1:
            for state in new_states:
                if state[3]+state[7]*2*(MAXT-1-t) >= max_poss_ge:
                    states.add(state)
        print(num, t, len(states), max_ge)
        
    ans *= max_ge
    if num == 3:
        break
print(ans)
