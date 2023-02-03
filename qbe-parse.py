# Parse QBE IR.
# Based on the doc: https://www.google.com/search?q=qbe+ir+reference&oq=qbe+ir+reference&aqs=chrome..69i57j33i10i160l2.5470j0j7&sourceid=chrome&ie=UTF-8

from pyparsing import *
from pyparsing.testing import pyparsing_test as ppt
from pyparsing.exceptions import ParseException
import re
import sys

ParserElement.set_default_whitespace_chars(' \t')

ident = Word(alphas + "_", alphanums + "_").set_name("ident")
integer = Word(nums).set_name("integer")
LBRACE, RBRACE, LPAR, RPAR, EQ, COMMA, SEMI = map(Suppress, "{}()=,;")
COMMENT = Char("#") + rest_of_line + '\n'
NL = Suppress(OneOrMore('\n' | COMMENT))

# https://c9x.me/compile/doc/il.html#Sigils
# is for user-defined Aggregate Types
user_type = Combine(':' + ident).set_name("user")
# is for globals (represented by a pointer)
global_ident = Combine('$' + ident).set_name("global")
# is for function-scope temporaries
temp = Combine('%' + ident).set_name("temp")
label = Combine('@' + ident).set_name("label")   # is for block labels

# https://c9x.me/compile/doc/il.html#Types
t_w = Char('w')
t_l = Char('l')
t_s = Char('s')
t_d = Char('d')
t_b = Char('b')
t_h = Char('h')
base_type = Char('wlsd').set_name("base_type")  # Base types
ext_type = (base_type | Char('bh')).set_name("ext_type")  # Extended types

# https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node19.html
sign = (Char('+') | Char('-')).set_name("sign")
exponent_marker = Char('esfdlESFDL')
exponent = Combine(exponent_marker + Optional(sign) +
                   integer).set_name("exponent")
float_num = (Combine(Optional(sign) + Optional(integer) + Char('.') +
                     integer + Optional(exponent)) | Combine(
    Optional(sign) + integer +
    Optional(Char('.') + integer)
    + exponent)).set_name("float")
single_float = Combine(Literal('s_') + float_num).set_name("single")
double_float = Combine(Literal('d_') + float_num).set_name("double")

value = (global_ident | temp | integer).set_name("value")

# https://c9x.me/compile/doc/il.html#Constants
const = (Combine(
    Optional('-') + integer) | single_float | double_float | global_ident).set_name("const")

# https://c9x.me/compile/doc/il.html#Linkage
secname = QuotedString('"').set_name("secname")
secflags = QuotedString('"').set_name("secflags")
linkage = ((Keyword("export") | (
    Keyword("section") + secname + Optional(secflags))) + Optional(NL)).set_name("linkage")

# https://c9x.me/compile/doc/il.html#Definitions

# Abstact types
sub_type = (ext_type | user_type).set_name("sub_type")
type_item = Group(ext_type("type") +
                  Optional(integer("repeat"))).set_name("type_item")
align = Keyword('align') + integer("align")
reg_type = (user_type("type_name") + EQ + Optional(align) + LBRACE + Group(
    delimited_list(type_item, delim=',', allow_trailing_delim=True))("items") + RBRACE).set_name("regtype")
opaque_type = (user_type("opaque_name") + EQ + align + LBRACE +
               integer("size") + RBRACE).set_name("opaque")
type_def = (Keyword("type") + ((reg_type | opaque_type))).set_name("type_def")

# Data
data_item = (Group((global_ident("symbol") + Optional(Suppress(Char('+')) + integer("offset"))
                    ))("global") | QuotedString('"')("string") | const("const")).set_name("data_item")
data_entry = ((ext_type("type") + (OneOrMore(Group(data_item)))("items")) |
              (Suppress(Literal('z')) + integer("zero_count"))).set_name("data_entry")
data_def = (ZeroOrMore(linkage) + Keyword("data") + global_ident +
            EQ + Optional(align) + LBRACE +
            Group(delimited_list(Group(data_entry), delim=',',
                                 allow_trailing_delim=True))("data_def") + RBRACE).set_name("data_ref")

block = Forward()

# Functions
abi_type = (base_type | user_type).set_name("abi_type")
param = ((abi_type + temp) | (Keyword('env') + temp)
         | Keyword("...")).set_name("param")
func_def = (ZeroOrMore(linkage)("linkage") + Keyword('function') + Optional(
    abi_type("return_type")) + global_ident("name") + LPAR + Group(delimited_list(
        Group(param), delim=',', allow_trailing_delim=True))("params") + RPAR + Optional(NL) + LBRACE + Optional(
            NL) + Group(ZeroOrMore(block))("body") + RBRACE + NL).set_name("func")

# https://c9x.me/compile/doc/il.html#Phi

phi = (temp("var") + Combine(EQ + base_type)("type") +
       Keyword('phi') + delimited_list(
    Group(label("label") + value("value")))("cases") + NL).set_name("phi")

# https://c9x.me/compile/doc/il.html#Instructions
t_T = Char('wlsd')
t_I = Char('wl')
t_F = Char('sd')
t_m = Char('l')   # assuming 64-bit arch


def inst1(name: str, ret: Char, p1: Char, group: list):
    prefix = temp("var") + Combine(EQ + ret)("type") if ret else Empty
    body = Keyword(name)("op") + value("p1") + NL
    inst = (prefix + body) if ret else body
    group.append(inst)
    return inst.set_name(name)


def inst2(name: str, ret: Char, p1: Char, p2: Char, group: list):
    prefix = temp("var") + Combine(EQ + ret)("type") if ret else Empty
    body = Keyword(name)("op") + value("p1") + COMMA + value("p2") + NL
    inst = (prefix + body) if ret else body
    group.append(inst)
    return inst.set_name(name)


# Arithmetic and Bits
arithmetic = []
i_add = inst2("add", t_T, t_T, t_T, arithmetic)
i_sub = inst2("sub", t_T, t_T, t_T, arithmetic)
i_div = inst2("div", t_T, t_T, t_T, arithmetic)
i_mul = inst2("mul", t_T, t_T, t_T, arithmetic)

i_neg = inst1("neg", t_T, t_T, arithmetic)

i_udiv = inst2("udiv", t_I, t_I, t_I, arithmetic)
i_rem = inst2("rem", t_I, t_I, t_I, arithmetic)
i_urem = inst2("urem", t_I, t_I, t_I, arithmetic)
i_or = inst2("or", t_I, t_I, t_I, arithmetic)
i_xor = inst2("xor", t_I, t_I, t_I, arithmetic)
i_and = inst2("and", t_I, t_I, t_I, arithmetic)
i_sar = inst2("sar", t_I, t_I, Char('ww'), arithmetic)
i_shr = inst2("shr", t_I, t_I, Char('ww'), arithmetic)
i_shl = inst2("shl", t_I, t_I, Char('ww'), arithmetic)

# https://c9x.me/compile/doc/il.html#Memory
mem_store = []
i_stored = inst2("stored", None, t_d, t_m, mem_store)
i_stores = inst2("stores", None, t_s, t_m, mem_store)
i_storel = inst2("storel", None, t_l, t_m, mem_store)
i_storew = inst2("storew", None, t_w, t_m, mem_store)
i_storeh = inst2("storeh", None, t_w, t_m, mem_store)
i_storeb = inst2("storeb", None, t_w, t_m, mem_store)

mem_load = []
t_mm = Char('mm')
i_loadd = inst1("loadd", t_d, t_m, mem_load)
i_loads = inst1("loads", t_s, t_m, mem_load)
i_loadl = inst1("loadl", t_l, t_m, mem_load)
i_loadw = inst1("loadw", t_I, t_m, mem_load)     # syntactic suger for i_loadsw
i_loadsw = inst1("loadsw", t_I, t_mm, mem_load)
i_loadsh = inst1("loadsh", t_I, t_mm, mem_load)
i_loadsb = inst1("loadsb", t_I, t_mm, mem_load)
i_loaduw = inst1("loaduw", t_I, t_mm, mem_load)
i_loaduh = inst1("loaduh", t_I, t_mm, mem_load)
i_loadub = inst1("loadub", t_I, t_mm, mem_load)

# Stack allocation. alloc4 is alloc bytes align on 4
stack_alloc = []
i_alloc4 = inst1("alloc4", t_m, t_l, stack_alloc)
i_alloc8 = inst1("alloc8", t_m, t_l, stack_alloc)
i_alloc16 = inst1("alloc16", t_m, t_l, stack_alloc)

# https://c9x.me/compile/doc/il.html#Comparisons

comparators = []


def BuildIntegerComparators():
    """
    eq for equality
    ne for inequality
    sle for signed lower or equal
    slt for signed lower
    sge for signed greater or equal
    sgt for signed greater
    ule for unsigned lower or equal
    ult for unsigned lower
    uge for unsigned greater or equal
    ugt for unsigned greater
    """
    comps = ["eq", "ne", "sle", "slt", "sge",
             "sgt", "ule", "ult", "uge", "ugt"]
    for c in comps:
        for t in "wl":
            comp = "c" + c + t
            name = "i_" + comp
            print(f"Adding {name}")
            globals()[name] = inst2(comp, t_I, Char(
                t + t), Char(t + t), comparators)


def BuildFloatComparators():
    """
    eq for equality
    ne for inequality
    le for lower or equal
    lt for lower
    ge for greater or equal
    gt for greater
    o for ordered (no operand is a NaN)
    uo for unordered (at least one operand is a NaN)
    """
    comps = ["eq", "ne", "le", "lt", "ge", "gt", "o", "uo"]
    for c in comps:
        for t in "ds":
            comp = "c" + c + t
            name = "i_" + comp
            print(f"Adding {name}")
            globals()[name] = inst2(comp, t_I, Char(
                t + t), Char(t + t), comparators)


BuildIntegerComparators()
BuildFloatComparators()

# https://c9x.me/compile/doc/il.html#Conversions
conversions = []
i_extsw = inst1("extsw", t_l, t_w, conversions)  # extend signed w -> l
i_extuw = inst1("extuw", t_l, t_w, conversions)  # extend unsigned w -> l
# extend signed h -> l/w
i_extsh = inst1("extsh", t_I, Char('ww'), conversions)
# extend unsigned h -> l/w
i_extuh = inst1("extuh", t_I, Char('ww'), conversions)
# extend signed b -> l/w
i_extsb = inst1("extsb", t_I, Char('ww'), conversions)
# extend unsigned b -> l/w
i_extub = inst1("extub", t_I, Char('ww'), conversions)
i_exts = inst1("exts", t_d, t_s, conversions)  # extend unsigned s -> d
i_truncd = inst1("truncd", t_s, t_d, conversions)    # trucate d -> s
# convert s -> signed  l/w
i_stosi = inst1("stosi", t_I, Char('ss'), conversions)
# convert s -> unsigned l/w
i_stoui = inst1("stoui", t_I, Char('ss'), conversions)
# convert d -> signed  l/w
i_dtosi = inst1("dtosi", t_I, Char('dd'), conversions)
# convert d -> unsigned l/w
i_dtoui = inst1("dtoui", t_I, Char('dd'), conversions)
# convert signed w -> d/s
i_swtof = inst1("swtof", t_F, Char('ww'), conversions)
# convert unsigned w -> d/s
i_uwtof = inst1("uwtof", t_F, Char('ww'), conversions)
# convert signed l -> d/s
i_sltof = inst1("sltof", t_F, Char('ll'), conversions)
# convert unsigned l -> d/s
i_ultof = inst1("ultof", t_F, Char('ll'), conversions)

instructions = arithmetic + mem_store + mem_load + \
    stack_alloc + comparators + conversions
instruct = MatchFirst(instructions)

# https://c9x.me/compile/doc/il.html#Control
# Even though not stated explicitly in the doc, jump is optoinal if the target is the next label.
jump = (Keyword("jmp") + label) | \
    (Keyword("jnz") + value + COMMA + label + COMMA + label) | \
    (Keyword("ret") + Optional(value))

block <<= label("label") + ZeroOrMore(phi)("phis") + \
    ZeroOrMore(instruct)("instrcutions") + Optional(jump)("jump") + NL

qbe_file = OneOrMore(type_def | data_def | func_def | block)


def Init():
    ParserElement.set_default_whitespace_chars(' \t')


data = """
@start
        %x =w copy 100
        %s =w copy 0
@loop
        %s =w add %s, %x
        %x =w sub %x, 1
        jnz %x, @loop, @end
@end
        ret %s
"""


def test_element(element: ParserElement, string: str, expected: str | list | None) -> bool:
    """
    element - the elemented to be tested
    string - input string
    expected - the expected output string or None if error is expected.
    """
    try:
        o = element.parse_string(string)
    except ParseException as e:
        if expected is None:
            print(
                f"test_element {element.name}: Success: Parse of '{string}' failed as expected.")
            return True
        else:
            print(
                f"test_element {element.name}: Error: Parse of '{string}' failed: '{str(e)}'.")
            return False

    result = o.as_list()[0] if type(expected) == str else o.as_list() if type(
        expected) == list else o.as_dict()
    if result != expected:
        print(
            f"test_element {element.name}: Error: Parse of '{string}' expected\n{expected}\ngot:\n{result}\nas list\n{o.as_list()}")
        return False
    print(f"test_element {element.name}: Success: '{string}' -> '{expected}'")
    return True


class TestCase(object):
    def __init__(self, name: str, element: ParserElement, string: str, expected: str | list | None):
        self.name = name
        self.element = element
        self.string = string
        self.expected = expected


def test_elements(tests: TestCase, stop_on_errors=False) -> int:
    errors: int = 0
    for t in tests:
        print(f"# Run test '{t.name}' on element <{t.element}>")
        if test_element(t.element, t.string, t.expected):
            continue
        errors += 1
        if stop_on_errors:
            return errors
    return errors


def Parse(s: str):
    Init()
    autoname_elements()
    block.set_debug()
    print(ppt.with_line_numbers(data))
    block.parseString(data)


if __name__ == "__main__":
    tests = [
        TestCase("simple", integer, "333", "333"),
        TestCase("single digit", integer, "2", "2"),
        TestCase("zero", integer, "00", "00"),
        TestCase("(Neg) negative", integer, "-1", None),

        TestCase("single char", ident, "a", "a"),
        TestCase("single capital char", ident, "B", "B"),
        TestCase("single char+num", ident, "a4", "a4"),
        TestCase("simple capital char+num", ident, "B1", "B1"),
        TestCase("Mix char+num", ident, "B1aa5", "B1aa5"),
        TestCase("Mix char+num+...+char", ident, "B1aa5z", "B1aa5z"),
        TestCase("Mix char+num+...+_", ident, "B1aa5_", "B1aa5_"),
        TestCase("Mix char+num+..._..+char", ident, "B1aa_5z", "B1aa_5z"),

        TestCase("user type", user_type, ":a", ":a"),
        TestCase("global type", global_ident, "$a", "$a"),
        TestCase("user type", temp, "%a4", "%a4"),
        TestCase("user type", label, "@a_", "@a_"),

        TestCase("sign -", sign, "-", "-"),
        TestCase("sign +", sign, "+", "+"),

        TestCase("exponent", exponent, "e1", "e1"),
        TestCase("exponent neg", exponent, "e-4", "e-4"),
        TestCase("exponent", exponent, "D1", "D1"),
        TestCase("exponent neg", exponent, "D-4", "D-4"),

        TestCase("simple", float_num, "1.0", "1.0"),
        TestCase("simple with sign", float_num, "-1.0", "-1.0"),
        TestCase("dot simple", float_num, ".1", ".1"),
        TestCase("dot simple with sign", float_num, "-.1", "-.1"),
        TestCase("exponet", float_num, "1e5", "1e5"),
        TestCase("exponet neg", float_num, "1e-5", "1e-5"),

        TestCase("single", single_float, "s_1.0", "s_1.0"),
        TestCase("double", double_float, "d_1.0e5", "d_1.0e5"),

        TestCase("value (global)", value, "$A8aa", "$A8aa"),
        TestCase("value (temp)", value, "%hello", "%hello"),
        TestCase("value (inter)", value, "44", "44"),

        TestCase("base word", base_type, "w", "w"),
        TestCase("base long", base_type, "l", "l"),
        TestCase("ext long", ext_type, "l", "l"),
        TestCase("ext half", ext_type, "h", "h"),

        TestCase("const (inter)", const, "67", "67"),
        TestCase("const (neg inter)", const, "-67", "-67"),
        TestCase("(NEG) const (neg space inter)", const, "- 67", None),
        TestCase("const (single)", const, "s_0.7", "s_0.7"),
        TestCase("const (double)", const, "d_0.7e6", "d_0.7e6"),

        TestCase("linkage export", linkage, "export", "export"),
        TestCase("linkage export + nl", linkage, "export  \n", "export"),
        TestCase("linkage export + comment", linkage,
                 "export # this is a comment \n", "export"),

        TestCase("linkage section", linkage,
                 'section "hello"', ['section', 'hello']),
        TestCase("linkage section + flags", linkage,
                 'section "hello" "flags"', ['section', 'hello', 'flags']),

        TestCase("linkage section + flags", linkage,
                 'section "hello" "flags"', ['section', 'hello', 'flags']),
        TestCase("linkage section + flags", linkage,
                 'section "hello" "flags"', ['section', 'hello', 'flags']),

        TestCase("reg_type single", reg_type,
                 ':fi1 = { h}', {'type_name': ':fi1', 'items': [{'type': 'h'}]}),
        TestCase("reg_type fourfloats", reg_type,
                 ':fourfloats = { s, s, d, d }', {'type_name': ':fourfloats', 'items': [{'type': 's'}, {'type': 's'}, {'type': 'd'}, {'type': 'd'}]}),
        TestCase("reg_type fourfloats", reg_type,
                 ':abyteandmanywords = { b, w 100 }', {'type_name': ':abyteandmanywords', 'items': [{'type': 'b'}, {'type': 'w', 'repeat': '100'}]}),
        TestCase("reg_type with align", reg_type,
                 ':cryptovector = align 16 { w 4 }', {'type_name': ':cryptovector', 'align': '16', 'items': [{'type': 'w', 'repeat': '4'}]}),
        TestCase("opaque_type with align", opaque_type,
                 ':opaque = align 16 { 32 }', {'opaque_name': ':opaque', 'align': '16', 'size': '32'}),
        TestCase("(NEG) opaque_type as reg", reg_type,
                 ':opaque = align 16 { 32 }', None),

        TestCase("typedef opaque", type_def,
                 'type :opaque = align 16 { 32 }', {'opaque_name': ':opaque', 'align': '16', 'size': '32'}),
        TestCase("typedef reg single", type_def,
                 'type :fi1 = { h} # a comment', {'type_name': ':fi1', 'items': [{'type': 'h'}]}),

        TestCase("data_item global", data_item, "$sym",
                 {"global": {"symbol": "$sym"}}),
        TestCase("data_item global+offset", data_item, "$sym + 12",
                 {"global": {"symbol": "$sym", "offset": "12"}}),
        TestCase("data_item string", data_item, '"kuku"', {"string": "kuku"}),
        TestCase("data_item const", data_item, '23', {"const": "23"}),


        TestCase("data_entry 3 words", data_entry, "w 1 2 3", {
                 "type": "w", "items": [{"const": "1"}, {"const": "2"}, {"const": "3"}]}),
        TestCase("data_entry clear bytes", data_entry,
                 "z 10", {"zero_count": "10"}),
        TestCase("data_entry long global", data_entry, "l $c", {
                 "type": "l", "items": [{"global": {"symbol": "$c"}}]}),

        TestCase("data_def, 2 type lists", data_def, "data $a = { w 1 2 3, b 0 }",
                 {"data_def": [{"type": "w", "items": [{"const": "1"}, {"const": "2"}, {"const": "3"}]},
                               {"type": "b", "items": [{"const": "0"}]}]}),
        TestCase("data_def clear bytes", data_def,
                 "data $b = {z 10}", {"data_def": [{"zero_count": "10"}]}),
        TestCase("data_def long neg const, long global", data_def, "data $c = { l -1, l $c }", {"data_def": [
            {"type": "l", "items": [{"const": "-1"}]},
            {"type": "l", "items": [{"global": {"symbol": "$c"}}]}]}),

        TestCase("abit_type base type", abi_type, "w", "w"),
        TestCase("abit_type user type", abi_type, ":u", ":u"),

        TestCase("param type + temp", param, "w %count", ["w", "%count"]),
        TestCase("param env + temp", param, "env %count", ["env", "%count"]),
        TestCase("param type + variadic", param, "...", "..."),

        TestCase("function ret + single user param", func_def, "function w $getone(:one %p) {}\n", {
            'linkage': [], 'return_type': 'w', 'name': '$getone', 'params': [
                [':one', '%p']], 'body': []}),
        TestCase("function export, ret + 4 params", func_def, "export function w $add(env %e, w %a, w %b) {}\n", {
            'linkage': ["export"], 'return_type': 'w', 'name': '$add', 'params': [
                ['env', '%e'], ['w', '%a'], ['w', '%b']], 'body': []}
        ),

        TestCase("phi ret + single user param", phi, "%y =w phi @ift 1, @iff 2\n", {'var': "%y", "type": "w", "cases": [
            {"label": "@ift", "value": "1"}, {"label": "@iff", "value": "2"}]}),


        # Arithmetics
        TestCase("add w + w", i_add, "%y =w add %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'add', 'p1': '%w', 'p2': '%y'}),
        TestCase("sub w + w", i_sub, "%y =w sub %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'sub', 'p1': '%w', 'p2': '%y'}),
        TestCase("div w + w", i_div, "%y =w div %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'div', 'p1': '%w', 'p2': '%y'}),
        TestCase("mul w + w", i_mul, "%y =w mul %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'mul', 'p1': '%w', 'p2': '%y'}),

        TestCase("neg w", i_neg, "%y =w neg %w\n", {
                 'var': '%y', 'type': 'w', 'op': 'neg', 'p1': '%w'}),

        TestCase("udiv w + w", i_udiv, "%y =w udiv %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'udiv', 'p1': '%w', 'p2': '%y'}),
        TestCase("rem w + w", i_rem, "%y =w rem %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'rem', 'p1': '%w', 'p2': '%y'}),
        TestCase("urem w + w", i_urem, "%y =w urem %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'urem', 'p1': '%w', 'p2': '%y'}),
        TestCase("or w + w", i_or, "%y =w or %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'or', 'p1': '%w', 'p2': '%y'}),
        TestCase("xor w + w", i_xor, "%y =w xor %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'xor', 'p1': '%w', 'p2': '%y'}),
        TestCase("and w + w", i_and, "%y =w and %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'and', 'p1': '%w', 'p2': '%y'}),

        TestCase("sar w + w", i_sar, "%y =w sar %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'sar', 'p1': '%w', 'p2': '%y'}),
        TestCase("shr w + w", i_shr, "%y =w shr %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'shr', 'p1': '%w', 'p2': '%y'}),
        TestCase("shl w + w", i_shl, "%y =w shl %w, %y\n",
                 {'var': '%y', 'type': 'w', 'op': 'shl', 'p1': '%w', 'p2': '%y'}),

        # mem stores
        TestCase("store d -> m", i_stored, "stored %w, %y\n",
                 {'op': 'stored', 'p1': '%w', 'p2': '%y'}),
        TestCase("store s -> m", i_stores, "stores %w, %y\n",
                 {'op': 'stores', 'p1': '%w', 'p2': '%y'}),
        TestCase("store l -> m", i_storel, "storel %w, %y\n",
                 {'op': 'storel', 'p1': '%w', 'p2': '%y'}),
        TestCase("store w -> m", i_storew, "storew %w, %y\n",
                 {'op': 'storew', 'p1': '%w', 'p2': '%y'}),
        TestCase("store h -> m", i_storeh, "storeh %w, %y\n",
                 {'op': 'storeh', 'p1': '%w', 'p2': '%y'}),
        TestCase("store b -> m", i_storeb, "storeb %w, %y\n",
                 {'op': 'storeb', 'p1': '%w', 'p2': '%y'}),

        # mem loads
        TestCase("load d -> m", i_loadd, "%z =d loadd %w\n",
                 {'var': '%z', 'type': 'd', 'op': 'loadd', 'p1': '%w'}),
        TestCase("load s -> m", i_loads, "%z =s loads %w\n",
                 {'var': '%z', 'type': 's', 'op': 'loads', 'p1': '%w'}),
        TestCase("load l -> m", i_loadl, "%z =l loadl %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'loadl', 'p1': '%w'}),
        TestCase("load w -> m", i_loadw, "%z =w loadw %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loadw', 'p1': '%w'}),
        TestCase("load sw -> m", i_loadsw, "%z =w loadsw %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loadsw', 'p1': '%w'}),
        TestCase("load sh -> m", i_loadsh, "%z =w loadsh %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loadsh', 'p1': '%w'}),
        TestCase("load sb -> m", i_loadsb, "%z =w loadsb %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loadsb', 'p1': '%w'}),
        TestCase("load uw -> m", i_loaduw, "%z =w loaduw %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loaduw', 'p1': '%w'}),
        TestCase("load uh -> m", i_loaduh, "%z =w loaduh %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loaduh', 'p1': '%w'}),
        TestCase("load ub -> m", i_loadub, "%z =w loadub %w\n",
                 {'var': '%z', 'type': 'w', 'op': 'loadub', 'p1': '%w'}),

        # stack allocs
        TestCase("alloc4 8", i_alloc4, "%z =l alloc4 %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'alloc4', 'p1': '%w'}),
        TestCase("alloc8 8", i_alloc8, "%z =l alloc8 %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'alloc8', 'p1': '%w'}),
        TestCase("alloc16 8", i_alloc16, "%z =l alloc16 %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'alloc16', 'p1': '%w'}),

        # Comparators are build by a builder functions, sample one of each builder
        TestCase("ceqw w w", i_ceqw, "%z =l ceqw %w, %y\n",
                 {'var': '%z', 'type': 'l', 'op': 'ceqw', 'p1': '%w', 'p2': '%y'}),
        TestCase("cod d d", i_cod, "%z =l cod %w, %y\n",
                 {'var': '%z', 'type': 'l', 'op': 'cod', 'p1': '%w', 'p2': '%y'}),

        # conversions
        TestCase("extend signed w", i_extsw, "%z =l extsw %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extsw', 'p1': '%w'}),
        TestCase("extend unsigned w", i_extuw, "%z =l extuw %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extuw', 'p1': '%w'}),
        TestCase("extend signed h", i_extsh, "%z =l extsh %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extsh', 'p1': '%w'}),
        TestCase("extend unsigned h", i_extuh, "%z =l extuh %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extuh', 'p1': '%w'}),
        TestCase("extend signed b", i_extsb, "%z =l extsb %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extsb', 'p1': '%w'}),
        TestCase("extend unsigned b", i_extub, "%z =l extub %w\n",
                 {'var': '%z', 'type': 'l', 'op': 'extub', 'p1': '%w'}),
        TestCase("extend s", i_exts, "%z =d exts %s\n",
                 {'var': '%z', 'type': 'd', 'op': 'exts', 'p1': '%s'}),
        TestCase("truncate d", i_truncd, "%z =s truncd %d\n",
                 {'var': '%z', 'type': 's', 'op': 'truncd', 'p1': '%d'}),
        TestCase("convert s to signed int", i_stosi, "%z =l stosi %s\n",
                 {'var': '%z', 'type': 'l', 'op': 'stosi', 'p1': '%s'}),
        TestCase("convert s to unsigned int", i_stoui, "%z =l stoui %s\n",
                 {'var': '%z', 'type': 'l', 'op': 'stoui', 'p1': '%s'}),
        TestCase("convert d to unsigned int", i_dtoui, "%z =l dtoui %d\n",
                 {'var': '%z', 'type': 'l', 'op': 'dtoui', 'p1': '%d'}),
        TestCase("convert d to unsigned int", i_dtoui, "%z =l dtoui %d\n",
                 {'var': '%z', 'type': 'l', 'op': 'dtoui', 'p1': '%d'}),
        TestCase("convert unsigned w to F", i_swtof, "%z =d swtof %w\n",
                 {'var': '%z', 'type': 'd', 'op': 'swtof', 'p1': '%w'}),
        TestCase("convert unsigned w to F", i_uwtof, "%z =d uwtof %w\n",
                 {'var': '%z', 'type': 'd', 'op': 'uwtof', 'p1': '%w'}),
        TestCase("convert unsigned l to F", i_sltof, "%z =d sltof %l\n",
                 {'var': '%z', 'type': 'd', 'op': 'sltof', 'p1': '%l'}),
        TestCase("convert unsigned l to F", i_ultof, "%z =d ultof %l\n",
                 {'var': '%z', 'type': 'd', 'op': 'ultof', 'p1': '%l'}),
    ]
    Init()
    errors = test_elements(tests)
    print("Success" if errors == 0 else f"*** Failed with {errors} errors.")
