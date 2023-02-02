# Parse QBE IR.
# Based on the doc: https://www.google.com/search?q=qbe+ir+reference&oq=qbe+ir+reference&aqs=chrome..69i57j33i10i160l2.5470j0j7&sourceid=chrome&ie=UTF-8

from pyparsing import *
from pyparsing.testing import pyparsing_test as ppt
from pyparsing.exceptions import ParseException
import re
import sys

ident = Word(alphas + "_", alphanums + "_").set_name("ident")
integer = Word(nums).set_name("integer")
LBRACE, RBRACE, LPAR, RPAR, EQ, COMMA, SEMI = map(Suppress, "{}()=,;")
COMMENT = Char("#") + rest_of_line + '\n'
NL = OneOrMore('\n' | COMMENT)

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
abi_type = base_type | user_type
param = (abi_type + temp) | (Keyword('env') + temp) | Keyword("...")
func_def = ZeroOrMore(linkage) + Keyword('function') + Optional(
    abi_type) + global_ident + LPAR + delimited_list(
        param, delim=',', allow_trailing_delim=True) + RPAR + Optional(NL) + LBRACE + Optional(
            NL) + ZeroOrMore(block) + RBRACE + NL

# https://c9x.me/compile/doc/il.html#Phi

phi = temp + EQ + base_type + \
    Keyword('phi') + label + value + ZeroOrMore(COMMA + label + value) + NL

# https://c9x.me/compile/doc/il.html#Instructions
t_T = 'wlsd'
t_I = 'wl'
t_F = 'sd'
t_m = 'l'   # assuming 64-bit arch

# Arithmetic and Bits
# add, sub, div, mul -- T(T,T)
# neg -- T(T)
# udiv, rem, urem -- I(I,I)
# or, xor, and -- I(I,I)
# sar, shr, shl -- I(I,ww)

ins_add = temp + Combine(EQ + t_T) + Keyword("add") + \
    value + COMMA + value + NL
ins_sub = temp + Combine(EQ + t_T) + Keyword("sub") + \
    value + COMMA + value + NL
ins_div = temp + Combine(EQ + t_T) + Keyword("div") + \
    value + COMMA + value + NL
ins_mul = temp + Combine(EQ + t_T) + Keyword("mul") + \
    value + COMMA + value + NL

ins_neg = temp + Combine(EQ + t_T) + Keyword("neg") + \
    value + COMMA + value + NL

# https://c9x.me/compile/doc/il.html#Memory
ins_stored = Keyword("stored") + value + COMMA + t_m + NL

# stored -- (d,m)
# stores -- (s,m)
# storel -- (l,m)
# storew -- (w,m)
# storeh -- (w,m)
# storeb -- (w,m)

ins_loadd = temp + Combine(EQ + t_d) + Keyword("neg") + \
    value + COMMA + value + NL
# loadd -- d(m)
# loads -- s(m)
# loadl -- l(m)
# loadsw, loaduw -- I(mm)
# loadsh, loaduh -- I(mm)
# loadsb, loadub -- I(mm)

instruct = (ins_add | ins_sub | ins_div | ins_mul |
            ins_neg | ins_stored | ins_loadd) + NL

# https://c9x.me/compile/doc/il.html#Control
# Even though not stated explicitly in the doc, jump is optoinal if the target is the next label.
jump = (Keyword("jmp") + label) | \
    (Keyword("jnz") + value + COMMA + label + COMMA + label) | \
    (Keyword("ret") + Optional(value))

block <<= label + ZeroOrMore(phi) + ZeroOrMore(instruct) + Optional(jump) + NL

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
    ]
    errors = test_elements(tests)
    print("Success" if errors == 0 else f"*** Failed with {errors} errors.")
