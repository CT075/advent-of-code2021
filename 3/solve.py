#!/usr/bin/python

with open("input.txt") as f:
    data = f.read()

lines = data.split()


def get(f, digit_order, candidates):
    for i in range(len(lines[0])):
        bits = list(map(list, zip(*candidates)))[i]
        selected = f(digit_order, key=bits.count)
        candidates = [n for n in candidates if n[i] == selected]
        if len(candidates) == 1:
            return int(candidates[0], 2)


oxy = get(max, "10", lines)
co2 = get(min, "01", lines)
print("oxygen", oxy)
print("co2", co2)
print("answer", oxy * co2)
