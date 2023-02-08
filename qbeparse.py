#!/usr/bin/env python3
# Parse QBE IR.
# Based on the doc: https://www.google.com/search?q=qbe+ir+reference&oq=qbe+ir+reference&aqs=chrome..69i57j33i10i160l2.5470j0j7&sourceid=chrome&ie=UTF-8

import argparse
import json
from pyparsing import *
from pyparsing.testing import pyparsing_test as ppt
import re
import sys

# Global constants
ELEM_DATA = "data"
ELEM_TYPE = "type"
ELEM_FUNC = "function"

# newline is not to be ignored!
ParserElement.set_default_whitespace_chars(' \t')
__diag__.enable_all_warnings()
#__diag__.enable_debug_on_named_expressions = True

ident = Word(alphas + "_" + '.', alphanums + "_" + ".").set_name("ident")
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
type_item = Group(sub_type("type") +
                  Optional(integer("repeat"))).set_name("type_item")
align = Keyword('align') + integer("align")
reg_type = (user_type("name") + EQ + Optional(align) + LBRACE + Group(
    delimited_list(type_item, delim=',', allow_trailing_delim=True))("members") + RBRACE).set_name("regtype")
# The union type is not documented but is practically used by cproc to encode c untions!
union_type = (user_type("name") + EQ + Optional(align) + LBRACE +
              Group((LBRACE + type_item + RBRACE) +
                    OneOrMore(LBRACE + type_item + RBRACE))("union") + RBRACE).set_name("untiontype")
opaque_type = (user_type("name") + EQ + align + LBRACE +
               integer("size") + RBRACE).set_name("opaque")
type_def = (Keyword(ELEM_TYPE)("elem") +
            ((reg_type | union_type | opaque_type)) + NL).set_name("type_def")

# Data
data_item = (Group((global_ident("symbol") + Optional(Suppress(Char('+')) + integer("offset"))
                    ))("global") | QuotedString('"')("string") | const("const")).set_name("data_item")
data_entry = ((ext_type("type") + (OneOrMore(Group(data_item)))("items")) |
              (Suppress(Literal('z')) + integer("zero_count"))).set_name("data_entry")
data_def = (ZeroOrMore(linkage) + Keyword(ELEM_DATA)("elem") + global_ident("name") +
            EQ + Optional(align) + LBRACE +
            Group(delimited_list(Group(data_entry), delim=',',
                                 allow_trailing_delim=True))("data_def") + RBRACE + NL).set_name("data_def")


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

# https://c9x.me/compile/doc/il.html#Cast-and-Copy
casts = []
# cast the param to the ret type (same size)
i_cast = inst1("cast", Char('wlsd'), Char('sdwl'), casts)
# copy the param to the dest (same type)
i_copy = inst1("copy", t_T, t_T, casts)


block = Forward()
# https://c9x.me/compile/doc/il.html#Functions
sub_word = (Literal('sb') | Literal('ub') | Literal(
    'sh') | Literal("uh")).set_name("sub_word")
abi_type = (base_type | sub_word | user_type).set_name("abi_type")
param = ((abi_type + temp) | (Keyword('env') + temp)
         | Literal("...")).set_name("param")
func_def = (ZeroOrMore(linkage)("linkage") + Keyword(ELEM_FUNC)("elem") + Optional(
    abi_type("return_type")) + global_ident("name") + LPAR + Optional(delimited_list(
        Group(param), delim=',', allow_trailing_delim=True))("params") + RPAR + Optional(NL) + LBRACE + Optional(
            NL) + Group(ZeroOrMore(Group(block)))("blocks") + RBRACE + NL).set_name("func")

# https://c9x.me/compile/doc/il.html#Call
# CALL := [%IDENT '=' ABITY] 'call' VAL '(' (ARG), ')'

# ARG :=
#     ABITY VAL  # Regular argument
#   | 'env' VAL  # Environment argument (first)
#   | '...'      # Variadic marker

# SUBWTY := 'sb' | 'ub' | 'sh' | 'uh'  # Sub-word types
# ABITY  := BASETY | SUBWTY | :IDENT
arg = ((abi_type + value) | (Literal('env') + value)
       | Literal("...")).set_name("arg")
call = (Optional(temp("ret_var") + Combine(EQ + abi_type)("ret_type")) + Keyword('call')("op") +
        value("name") + LPAR + delimited_list(Group(arg))("args") + RPAR + NL).set_name("call")

instructions = arithmetic + mem_store + mem_load + \
    stack_alloc + comparators + conversions + casts
instruct = MatchFirst(instructions) | call

# https://c9x.me/compile/doc/il.html#Control

# https://c9x.me/compile/doc/il.html#Jumps
jump = (((Keyword("jmp")("jump") + label("target")) |
         (Keyword("jnz")("jump") + value("test") + COMMA + label("notzero") + COMMA + label("zero")) |
         (Keyword("ret")("jump") + Optional(value)("ret_value")) |
         Keyword('hlt')("jump")) + NL).set_name("jump")

# https://c9x.me/compile/doc/il.html#Blocks
# Even though not stated explicitly in the doc, the jump part of the block is optional
# if the target is the next label.
block <<= (Optional(NL) + label("label") + NL + Group(ZeroOrMore(Group(phi)))("phis") +
           Group(ZeroOrMore((Group(instruct))))("inst") + Group(Optional(jump))("jump")).set_name("block")

top = (Group(func_def) | Group(type_def) | Group(data_def)).set_name("top")
qbe_file = Group(OneOrMore(top))("elems").set_name("qbe")


def ParseText(s: str, verbose: bool = False, debug: bool = False) -> ParserElement:
    # autoname_elements()
    qbe_file.set_debug(debug)
    func_def.set_debug(debug)
    type_def.set_debug(debug)
    data_def.set_debug(debug)
    block.set_debug(debug)
    if verbose:
        print(ppt.with_line_numbers(s))
    return qbe_file.parseString(s)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='qbeparse',
        description='Parse qbe files.',
        epilog='')
    parser.add_argument('filename', metavar='Filename',
                        nargs='+',)           # positional argument
    parser.add_argument('-v', '--verbose', action='store_true')  # on/off flag
    parser.add_argument('-d', '--debug', action='store_true')  # on/off flag
    args = parser.parse_args()
    for f in args.filename:
        try:
            print(f"------ '{f}' ------\n")
            json_dict = ParseText(open(f, "r").read() +
                                  "\n", verbose=args.verbose, debug=args.debug).as_dict()
            json.dump(json_dict, sys.stdout, indent=4, sort_keys=False)
        except ParseException as e:
            print(f"Parsing of '{f}' failed: {str(e)}", file=sys.stderr)
        except IOError as e:
            print(f"Can't access/read '{f}': {str(e)}", file=sys.stderr)
    print()
    sys.exit(0)
