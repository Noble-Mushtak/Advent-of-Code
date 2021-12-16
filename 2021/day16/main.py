import sys

def getnum(ch):
    if ord(ch) >= ord("0") and ord(ch) <= ord("9"):
        return bin(ord(ch)-ord("0"))[2:].zfill(4)
    if ord(ch) >= ord("A") and ord(ch) <= ord("F"):
        return bin(ord(ch)-ord("A")+10)[2:].zfill(4)

with open(sys.argv[1], "r") as file:
    lines = file.read().strip().split("\n")
input = "".join(getnum(ch) for ch in lines[0])

cur_bit = 0

def read_bits(n):
    global cur_bit
    num = 0
    for i in range(n):
        num <<= 1
        num += ord(input[cur_bit])-ord("0")
        cur_bit += 1
    return num

def process(ti, args):
    if ti == 0: return sum(args)
    if ti == 1:
        ans = 1
        for n in args: ans *= n
        return ans
    if ti == 2: return min(args)
    if ti == 3: return max(args)
    if ti == 5: return int(args[0] > args[1])
    if ti == 6: return int(args[0] < args[1])
    if ti == 7: return int(args[0] == args[1])

def parse_packet():
    version = read_bits(3)
    type_id = read_bits(3)
    before_bits = 3+3
    if type_id == 4:
        num_bits = 0
        true_num = 0
        while True:
            num = read_bits(5)
            num_bits += 5
            true_num <<= 4
            if num < 16:
                true_num += num
                break
            else:
                true_num += num-16
        return version, true_num, before_bits+num_bits
    else:
        length_type_id = read_bits(1)
        before_bits += 1
        num_bits = 0
        num_subpackets = 0
        if length_type_id == 0:
            num_bits = read_bits(15)
            before_bits += 15
        else:
            num_subpackets = read_bits(11)
            before_bits += 11
        cur_bits = 0
        cur_subpackets = 0
        args = []
        while True:
            if length_type_id == 0 and cur_bits == num_bits:
                break
            if length_type_id == 1 and cur_subpackets == num_subpackets:
                break
            v, n, cb = parse_packet()
            args.append(n)
            cur_bits += cb
            version += v
            cur_subpackets += 1
        return version, process(type_id, args), before_bits+cur_bits
        
version_sum, result, _ = parse_packet()
print(f"Part 1: { version_sum }")
print(f"Part 2: { result }")
