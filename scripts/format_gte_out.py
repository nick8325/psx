# Converts a GTE dump into human-readable format.
# Can also convert a GTE dump into a test case for
# ps1-tests/gte/test-all.
#
# The dump should consist of lines of the following format,
# separated by "--" lines:
#
# 1234abcd
# 01234567 12303546 1abcdef0 ...
# 1123456e aaaaaaaa 1abcdef0 ...
#
# where:
# * the first line is the opcode
# * the second line gives the GTE registers before executing the opcode
# * the third line gives the GTE registers after executing the opcode

import sys
from itertools import islice

regs = [
    "VXY0", "VZ0", "VXY1", "VZ1", "VXY2", "VZ2", "RGBC", "OTZ", "IR0", "IR1", "IR2", "IR3",
    "SXY0", "SXY1", "SXY2", "SXYP", "SZ0", "SZ1", "SZ2", "SZ3", "RGB0", "RGB1", "RGB2",
    "RES1", "MAC0", "MAC1", "MAC2", "MAC3", "IRGB", "ORGB", "LZCS", "LZCR",
    "RT1", "RT2", "RT3", "RT4", "RT5", "TRX", "TRY", "TRZ", "LL1", "LL2", "LL3", "LL4", "LL5",
    "RBC", "GBC", "BBC", "LC1", "LC2", "LC3", "LC4", "LC5", "RFC", "GFC", "BFC",
    "OFX", "OFY", "H", "DQA", "DQB", "ZSF3", "ZSF4", "FLAG"
]

max_width = max(map(len, regs))

def filtered_lines(file):
    for line in file.readlines():
        line = line.strip()
        if not line: continue
        if line.startswith("args"): continue
        if line[0].isdigit() or line[0] in "abcdef" or line == "--":
            yield line

def hex_num(string):
    return int(string, base=16)

def map_list(f, lst):
    return list(map(f, lst))

def reg_dict(vals):
    return {reg: hex_num(val) for reg, val in zip(regs, vals)}

def tests(file):
    iterator = filtered_lines(file)
    while True:
        try:
            instruction = next(iterator)
        except StopIteration:
            return
        before = next(iterator).split()
        after = next(iterator).split()
        dashes = next(iterator)
        assert dashes == "--"
        yield hex_num(instruction), reg_dict(before), reg_dict(after)

def chunk(indent, items):
    result = []
    line = ""
    for item in items:
        line += " " + item
        if len(line) >= 80-indent:
            result.append("  " * indent + line)
            line = ""
    if line:
        result.append("  " * indent + line)
    return "\n".join(result)

def str_regs(indent, regs):
    return chunk(indent,
        (reg.rjust(max_width) + ": " + f"{val:08x}" 
         for reg, val in regs.items()))

def str_regs_c(indent, regs):
    vals = list(f"0x{val:08x}" for val in regs.values())
    for i in range(len(vals)-1):
        vals[i] += ","
    return chunk(indent, vals)

def str_diff(indent, regs1, regs2):
    return chunk(indent,
        (reg.rjust(max_width) + ": " + f"{val1:08x} => {regs2[reg]:08x}"
         for reg, val1 in regs1.items() if regs2[reg] != val1))

# Main entry point.
# Pretty-prints tests, reading from stdin.
def prettify_tests():
    for i, (instruction, before, after) in enumerate(tests(sys.stdin)):
        instruction_cop = instruction & 0x01ffffff
        print(f"""\
Test {i}:
Instruction = {instruction_cop:08x}
Before:
{str_regs(2, before)}
After:
{str_diff(2, before, after)}
""")

# Alternative entry point.
# Converts the given test to ps1-tests/gte/test-all format.
def c_test(test_number):
    [test] = islice(tests(sys.stdin), test_number, test_number+1)
    instruction, before, after = test
    instruction_cop = instruction & 0x01ffffff

    print("""\
{
    .name = "amitest",
    .input = {
%s
    },
    .opcode = 0x%08x,
    .output = {
%s
    }
}""" % (str_regs_c(8, before), instruction_cop, str_regs_c(8, after)))

prettify_tests()
