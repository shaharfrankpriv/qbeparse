#!/usr/bin/env python3
import qbeparse as qbe
import unittest
from pyparsing.exceptions import ParseException
from pyparsing import ParserElement


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


class TestQBEParsing(unittest.TestCase):

    def test_integer(self):
        tests = [
            TestCase("integer simple", qbe.integer, "333", "333"),
            TestCase("integer single digit", qbe.integer, "2", "2"),
            TestCase("integer zero", qbe.integer, "00", "00"),
            TestCase("(Neg) integer negative", qbe.integer, "-1", None),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_ident(self):
        tests = [
            TestCase("ident - single char", qbe.ident, "a", "a"),
            TestCase("ident - single capital char", qbe.ident, "B", "B"),
            TestCase("ident - single char+num", qbe.ident, "a4", "a4"),
            TestCase("ident - simple capital char+num", qbe.ident, "B1", "B1"),
            TestCase("ident - Mix char+num", qbe.ident, "B1aa5", "B1aa5"),
            TestCase("ident - Mix char+num+...+char",
                     qbe.ident, "B1aa5z", "B1aa5z"),
            TestCase("ident - Mix char+num+...+_",
                     qbe.ident, "B1aa5_", "B1aa5_"),
            TestCase("ident - Mix char+num+..._..+char",
                     qbe.ident, "B1aa_5z", "B1aa_5z"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_types(self):
        tests = [
            TestCase("user type", qbe.user_type, ":a", ":a"),
            TestCase("global type", qbe.global_ident, "$a", "$a"),
            TestCase("temp type", qbe.temp, "%a4", "%a4"),
            TestCase("label type", qbe.label, "@a_", "@a_"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_floats(self):
        tests = [
            TestCase("sign -", qbe.sign, "-", "-"),
            TestCase("sign +", qbe.sign, "+", "+"),

            TestCase("exponent", qbe.exponent, "e1", "e1"),
            TestCase("exponent neg", qbe.exponent, "e-4", "e-4"),
            TestCase("exponent", qbe.exponent, "D1", "D1"),
            TestCase("exponent neg", qbe.exponent, "D-4", "D-4"),

            TestCase("float simple", qbe.float_num, "1.0", "1.0"),
            TestCase("float simple with sign", qbe.float_num, "-1.0", "-1.0"),
            TestCase("float - dot simple", qbe.float_num, ".1", ".1"),
            TestCase("float - dot simple with sign",
                     qbe.float_num, "-.1", "-.1"),
            TestCase("float - exponet", qbe.float_num, "1e5", "1e5"),
            TestCase("float - exponet neg", qbe.float_num, "1e-5", "1e-5"),

            TestCase("single", qbe.single_float, "s_1.0", "s_1.0"),
            TestCase("double", qbe.double_float, "d_1.0e5", "d_1.0e5"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_value(self):
        tests = [
            TestCase("value - global", qbe.value, "$A8aa", "$A8aa"),
            TestCase("value - temp", qbe.value, "%hello", "%hello"),
            TestCase("value - inter", qbe.value, "44", "44"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_type_char(self):
        tests = [
            TestCase("base word", qbe.base_type, "w", "w"),
            TestCase("base long", qbe.base_type, "l", "l"),
            TestCase("ext long", qbe.ext_type, "l", "l"),
            TestCase("ext half", qbe.ext_type, "h", "h"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_const(self):
        tests = [
            TestCase("const (inter)", qbe.const, "67", "67"),
            TestCase("const (neg inter)", qbe.const, "-67", "-67"),
            TestCase("(NEG) const (neg space inter)", qbe.const, "- 67", None),
            TestCase("const (single)", qbe.const, "s_0.7", "s_0.7"),
            TestCase("const (double)", qbe.const, "d_0.7e6", "d_0.7e6"),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_linkage(self):
        tests = [
            TestCase("linkage export", qbe.linkage, "export", "export"),
            TestCase("linkage export + nl", qbe.linkage,
                     "export  \n", "export"),
            TestCase("linkage export + comment", qbe.linkage,
                     "export # this is a comment \n", "export"),

            TestCase("linkage section", qbe.linkage,
                     'section "hello"', ['section', 'hello']),
            TestCase("linkage section + flags", qbe.linkage,
                     'section "hello" "flags"', ['section', 'hello', 'flags']),

            TestCase("linkage section + flags", qbe.linkage,
                     'section "hello" "flags"', ['section', 'hello', 'flags']),
            TestCase("linkage section + flags", qbe.linkage,
                     'section "hello" "flags"', ['section', 'hello', 'flags']),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_typedef(self):
        tests = [
            TestCase("reg_type single", qbe.reg_type,
                     ':fi1 = { h}', {'type_name': ':fi1', 'items': [{'type': 'h'}]}),
            TestCase("reg_type fourfloats", qbe.reg_type,
                     ':fourfloats = { s, s, d, d }', {'type_name': ':fourfloats', 'items': [{'type': 's'}, {'type': 's'}, {'type': 'd'}, {'type': 'd'}]}),
            TestCase("reg_type fourfloats", qbe.reg_type,
                     ':abyteandmanywords = { b, w 100 }', {'type_name': ':abyteandmanywords', 'items': [{'type': 'b'}, {'type': 'w', 'repeat': '100'}]}),
            TestCase("reg_type with align", qbe.reg_type,
                     ':cryptovector = align 16 { w 4 }', {'type_name': ':cryptovector', 'align': '16', 'items': [{'type': 'w', 'repeat': '4'}]}),
            TestCase("opaque_type with align", qbe.opaque_type,
                     ':opaque = align 16 { 32 }', {'opaque_name': ':opaque', 'align': '16', 'size': '32'}),
            TestCase("(NEG) opaque_type as reg", qbe.reg_type,
                     ':opaque = align 16 { 32 }', None),

            TestCase("typedef opaque", qbe.type_def,
                     'type :opaque = align 16 { 32 }', {'opaque_name': ':opaque', 'align': '16', 'size': '32'}),
            TestCase("typedef reg single", qbe.type_def,
                     'type :fi1 = { h} # a comment', {'type_name': ':fi1', 'items': [{'type': 'h'}]}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_datadef(self):
        tests = [
            TestCase("data_item global", qbe.data_item, "$sym",
                     {"global": {"symbol": "$sym"}}),
            TestCase("data_item global+offset", qbe.data_item, "$sym + 12",
                     {"global": {"symbol": "$sym", "offset": "12"}}),
            TestCase("data_item string", qbe.data_item,
                     '"kuku"', {"string": "kuku"}),
            TestCase("data_item const", qbe.data_item, '23', {"const": "23"}),


            TestCase("data_entry 3 words", qbe.data_entry, "w 1 2 3", {
                "type": "w", "items": [{"const": "1"}, {"const": "2"}, {"const": "3"}]}),
            TestCase("data_entry clear bytes", qbe.data_entry,
                     "z 10", {"zero_count": "10"}),
            TestCase("data_entry long global", qbe.data_entry, "l $c", {
                "type": "l", "items": [{"global": {"symbol": "$c"}}]}),

            TestCase("data_def, 2 type lists", qbe.data_def, "data $a = { w 1 2 3, b 0 }",
                     {"data_def": [{"type": "w", "items": [{"const": "1"}, {"const": "2"}, {"const": "3"}]},
                                   {"type": "b", "items": [{"const": "0"}]}]}),
            TestCase("data_def clear bytes", qbe.data_def,
                     "data $b = {z 10}", {"data_def": [{"zero_count": "10"}]}),
            TestCase("data_def long neg const, long global", qbe.data_def, "data $c = { l -1, l $c }", {"data_def": [
                {"type": "l", "items": [{"const": "-1"}]},
                {"type": "l", "items": [{"global": {"symbol": "$c"}}]}]}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_arithmetics(self):
        tests = [
            TestCase("add w + w", qbe.i_add, "%y =w add %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'add', 'p1': '%w', 'p2': '%y'}),
            TestCase("sub w + w", qbe.i_sub, "%y =w sub %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'sub', 'p1': '%w', 'p2': '%y'}),
            TestCase("div w + w", qbe.i_div, "%y =w div %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'div', 'p1': '%w', 'p2': '%y'}),
            TestCase("mul w + w", qbe.i_mul, "%y =w mul %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'mul', 'p1': '%w', 'p2': '%y'}),

            TestCase("neg w", qbe.i_neg, "%y =w neg %w\n", {
                'var': '%y', 'type': 'w', 'op': 'neg', 'p1': '%w'}),

            TestCase("udiv w + w", qbe.i_udiv, "%y =w udiv %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'udiv', 'p1': '%w', 'p2': '%y'}),
            TestCase("rem w + w", qbe.i_rem, "%y =w rem %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'rem', 'p1': '%w', 'p2': '%y'}),
            TestCase("urem w + w", qbe.i_urem, "%y =w urem %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'urem', 'p1': '%w', 'p2': '%y'}),
            TestCase("or w + w", qbe.i_or, "%y =w or %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'or', 'p1': '%w', 'p2': '%y'}),
            TestCase("xor w + w", qbe.i_xor, "%y =w xor %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'xor', 'p1': '%w', 'p2': '%y'}),
            TestCase("and w + w", qbe.i_and, "%y =w and %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'and', 'p1': '%w', 'p2': '%y'}),

            TestCase("sar w + w", qbe.i_sar, "%y =w sar %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'sar', 'p1': '%w', 'p2': '%y'}),
            TestCase("shr w + w", qbe.i_shr, "%y =w shr %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'shr', 'p1': '%w', 'p2': '%y'}),
            TestCase("shl w + w", qbe.i_shl, "%y =w shl %w, %y\n",
                     {'var': '%y', 'type': 'w', 'op': 'shl', 'p1': '%w', 'p2': '%y'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_memstores(self):
        tests = [
            TestCase("store d -> m", qbe.i_stored, "stored %w, %y\n",
                     {'op': 'stored', 'p1': '%w', 'p2': '%y'}),
            TestCase("store s -> m", qbe.i_stores, "stores %w, %y\n",
                     {'op': 'stores', 'p1': '%w', 'p2': '%y'}),
            TestCase("store l -> m", qbe.i_storel, "storel %w, %y\n",
                     {'op': 'storel', 'p1': '%w', 'p2': '%y'}),
            TestCase("store w -> m", qbe.i_storew, "storew %w, %y\n",
                     {'op': 'storew', 'p1': '%w', 'p2': '%y'}),
            TestCase("store h -> m", qbe.i_storeh, "storeh %w, %y\n",
                     {'op': 'storeh', 'p1': '%w', 'p2': '%y'}),
            TestCase("store b -> m", qbe.i_storeb, "storeb %w, %y\n",
                     {'op': 'storeb', 'p1': '%w', 'p2': '%y'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_memloads(self):
        tests = [
            TestCase("load d -> m", qbe.i_loadd, "%z =d loadd %w\n",
                     {'var': '%z', 'type': 'd', 'op': 'loadd', 'p1': '%w'}),
            TestCase("load s -> m", qbe.i_loads, "%z =s loads %w\n",
                     {'var': '%z', 'type': 's', 'op': 'loads', 'p1': '%w'}),
            TestCase("load l -> m", qbe.i_loadl, "%z =l loadl %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'loadl', 'p1': '%w'}),
            TestCase("load w -> m", qbe.i_loadw, "%z =w loadw %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loadw', 'p1': '%w'}),
            TestCase("load sw -> m", qbe.i_loadsw, "%z =w loadsw %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loadsw', 'p1': '%w'}),
            TestCase("load sh -> m", qbe.i_loadsh, "%z =w loadsh %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loadsh', 'p1': '%w'}),
            TestCase("load sb -> m", qbe.i_loadsb, "%z =w loadsb %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loadsb', 'p1': '%w'}),
            TestCase("load uw -> m", qbe.i_loaduw, "%z =w loaduw %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loaduw', 'p1': '%w'}),
            TestCase("load uh -> m", qbe.i_loaduh, "%z =w loaduh %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loaduh', 'p1': '%w'}),
            TestCase("load ub -> m", qbe.i_loadub, "%z =w loadub %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'loadub', 'p1': '%w'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_alloc(self):
        tests = [
            TestCase("alloc4 8", qbe.i_alloc4, "%z =l alloc4 %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'alloc4', 'p1': '%w'}),
            TestCase("alloc8 8", qbe.i_alloc8, "%z =l alloc8 %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'alloc8', 'p1': '%w'}),
            TestCase("alloc16 8", qbe.i_alloc16, "%z =l alloc16 %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'alloc16', 'p1': '%w'}),

        ]
        self.assertEqual(test_elements(tests), 0)

    def test_comparators(self):
        tests = [
            # Comparators are build by a builder functions, sample one of each builder
            TestCase("ceqw w w", qbe.i_ceqw, "%z =l ceqw %w, %y\n",
                     {'var': '%z', 'type': 'l', 'op': 'ceqw', 'p1': '%w', 'p2': '%y'}),
            TestCase("cod d d", qbe.i_cod, "%z =l cod %w, %y\n",
                     {'var': '%z', 'type': 'l', 'op': 'cod', 'p1': '%w', 'p2': '%y'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_conversions(self):
        tests = [
            # conversions
            TestCase("extend signed w", qbe.i_extsw, "%z =l extsw %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extsw', 'p1': '%w'}),
            TestCase("extend unsigned w", qbe.i_extuw, "%z =l extuw %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extuw', 'p1': '%w'}),
            TestCase("extend signed h", qbe.i_extsh, "%z =l extsh %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extsh', 'p1': '%w'}),
            TestCase("extend unsigned h", qbe.i_extuh, "%z =l extuh %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extuh', 'p1': '%w'}),
            TestCase("extend signed b", qbe.i_extsb, "%z =l extsb %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extsb', 'p1': '%w'}),
            TestCase("extend unsigned b", qbe.i_extub, "%z =l extub %w\n",
                     {'var': '%z', 'type': 'l', 'op': 'extub', 'p1': '%w'}),
            TestCase("extend s", qbe.i_exts, "%z =d exts %s\n",
                     {'var': '%z', 'type': 'd', 'op': 'exts', 'p1': '%s'}),
            TestCase("truncate d", qbe.i_truncd, "%z =s truncd %d\n",
                     {'var': '%z', 'type': 's', 'op': 'truncd', 'p1': '%d'}),
            TestCase("convert s to signed int", qbe.i_stosi, "%z =l stosi %s\n",
                     {'var': '%z', 'type': 'l', 'op': 'stosi', 'p1': '%s'}),
            TestCase("convert s to unsigned int", qbe.i_stoui, "%z =l stoui %s\n",
                     {'var': '%z', 'type': 'l', 'op': 'stoui', 'p1': '%s'}),
            TestCase("convert d to unsigned int", qbe.i_dtoui, "%z =l dtoui %d\n",
                     {'var': '%z', 'type': 'l', 'op': 'dtoui', 'p1': '%d'}),
            TestCase("convert d to unsigned int", qbe.i_dtoui, "%z =l dtoui %d\n",
                     {'var': '%z', 'type': 'l', 'op': 'dtoui', 'p1': '%d'}),
            TestCase("convert unsigned w to F", qbe.i_swtof, "%z =d swtof %w\n",
                     {'var': '%z', 'type': 'd', 'op': 'swtof', 'p1': '%w'}),
            TestCase("convert unsigned w to F", qbe.i_uwtof, "%z =d uwtof %w\n",
                     {'var': '%z', 'type': 'd', 'op': 'uwtof', 'p1': '%w'}),
            TestCase("convert unsigned l to F", qbe.i_sltof, "%z =d sltof %l\n",
                     {'var': '%z', 'type': 'd', 'op': 'sltof', 'p1': '%l'}),
            TestCase("convert unsigned l to F", qbe.i_ultof, "%z =d ultof %l\n",
                     {'var': '%z', 'type': 'd', 'op': 'ultof', 'p1': '%l'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_casts(self):
        tests = [
            TestCase("cast w -> s", qbe.i_cast, "%z =s cast %w\n",
                     {'var': '%z', 'type': 's', 'op': 'cast', 'p1': '%w'}),
            TestCase("copy w -> w", qbe.i_copy, "%z =w copy %w\n",
                     {'var': '%z', 'type': 'w', 'op': 'copy', 'p1': '%w'}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_call(self):
        tests = [
            TestCase("arg type + const", qbe.arg, "w 5", ["w", "5"]),
            TestCase("arg env + temp", qbe.arg,
                     "env %count", ["env", "%count"]),
            TestCase("arg type + variadic", qbe.arg, "...", "..."),
            TestCase("call - no ret, single arg", qbe.call, "call $incone(w %p)\n", {
                'name': "$incone", 'op': 'call', 'args': [['w', '%p']], }),
            TestCase("call - ret + two arg", qbe.call, "%r =s call $vadd(s %a, l %ap)\n", {
                'ret_var': '%r', 'op': 'call', 'ret_type': 's', 'name': "$vadd", 'args': [['s', '%a'], ['l', '%ap']], }),
            TestCase("call - ret + even + two arg", qbe.call, "%r =s call $vadd(env %b, s %a, l %ap)\n", {
                'ret_var': '%r', 'op': 'call', 'ret_type': 's', 'name': "$vadd", 'args': [['env', '%b'], ['s', '%a'], ['l', '%ap']], }),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_jump(self):
        tests = [
            TestCase("jump - unconditonal", qbe.jump,
                     "jmp @loop\n", ["jmp", "@loop"]),
            TestCase("jump - unconditonal (dir)", qbe.jump, "jmp @loop\n",
                     {"jump": "jmp", "target": "@loop"}),
            TestCase("jump - conditonal", qbe.jump, "jnz %x, @loop, @end\n", {
                "jump": "jnz", "test": "%x", "notzero": "@loop", "zero": "@end"}),
            TestCase("jump/ret - no value", qbe.jump, "ret\n",
                     {"jump": "ret"}),
            TestCase("jump/ret - const value", qbe.jump, "ret 8\n",
                     {"jump": "ret", "ret_value": "8"}),
            TestCase("jump/ret - var value", qbe.jump, "ret %y\n",
                     {"jump": "ret", "ret_value": "%y"}),
            TestCase("jump/htl (termination)", qbe.jump,
                     "hlt\n", {"jump": "hlt"}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_phi(self):
        tests = [
            TestCase("phi ret + single user param", qbe.phi, "%y =w phi @ift 1, @iff 2\n", {'var': "%y", "type": "w", "cases": [
                {"label": "@ift", "value": "1"}, {"label": "@iff", "value": "2"}]}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_block(self):
        tests = [
            TestCase("block - simple, no phis no jump", qbe.block, '@start\n\t%x =w copy 100\n\t%y =w copy 0\n', {
                'label': '@start', 'phis': [], 'inst': [{'var': '%x', 'type': 'w', 'op': 'copy', 'p1': '100'},
                                                        {'var': '%y', 'type': 'w', 'op': 'copy', 'p1': '0'}],
                "jump": []}),
            TestCase("block - one phis, one inst, end with jump", qbe.block,
                     """@loop
                        %x =w phi @start 100, @loop %x1
                        %x1 =w sub %x, 1
                        jnz %x1, @loop, @end
                    """, {
                         'label': '@loop', 'phis': [{'var': "%x", "type": "w", "cases": [
                             {"label": "@start", "value": "100"}, {"label": "@loop", "value": "%x1"}]}],
                         'inst': [{'var': '%x1', 'type': 'w', 'op': 'sub', 'p1': '%x', 'p2': '1'}],
                         "jump": {"jump": "jnz", "test": "%x1", "notzero": "@loop", "zero": "@end"}}),

            TestCase("block - two phis, two inst, end with jump", qbe.block,
                     """@loop
                        %x =w phi @start 100, @loop %x1
                        %y =l phi @start 30, @loop %x2
                        %x1 =w sub %x, 1
                        %x2 =w mul %y, 2
                        jnz %x1, @loop, @end
                    """, {
                         'label': '@loop', 'phis': [{'var': "%x", "type": "w", "cases": [
                             {"label": "@start", "value": "100"}, {"label": "@loop", "value": "%x1"}]},
                             {'var': "%y", "type": "l", "cases": [
                                 {"label": "@start", "value": "30"}, {"label": "@loop", "value": "%x2"}]}],
                         'inst': [{'var': '%x1', 'type': 'w', 'op': 'sub', 'p1': '%x', 'p2': '1'},
                                  {'var': '%x2', 'type': 'w', 'op': 'mul', 'p1': '%y', 'p2': '2'}],
                         "jump": {"jump": "jnz", "test": "%x1", "notzero": "@loop", "zero": "@end"}}),
        ]
        self.assertEqual(test_elements(tests), 0)

    def test_function(self):
        f1 = '''function $loop() {
                    @start
                    @loop
                            %x =w phi @start 100, @loop %x1
                            %x1 =w sub %x, 1
                            jnz %x1, @loop, @end
                    @end
                            ret
                    }
                    '''
        tests = [
            TestCase("abi_type base type", qbe.abi_type, "w", "w"),
            TestCase("abi_type user type", qbe.abi_type, ":u", ":u"),

            TestCase("param type + temp", qbe.param,
                     "w %count", ["w", "%count"]),
            TestCase("param env + temp", qbe.param,
                     "env %count", ["env", "%count"]),
            TestCase("param type + variadic", qbe.param, "...", "..."),

            TestCase("function ret + single user param", qbe.func_def, "function w $getone(:one %p) {}\n", {
                'linkage': [], 'return_type': 'w', 'name': '$getone', 'params': [
                    [':one', '%p']], 'blocks': []}),
            TestCase("function export, ret + 4 params", qbe.func_def, "export function w $add(env %e, w %a, w %b) {}\n", {
                'linkage': ["export"], 'return_type': 'w', 'name': '$add', 'params': [
                    ['env', '%e'], ['w', '%a'], ['w', '%b']], 'blocks': []}
            ),
            TestCase("function f1, no param, with 3 blocks", qbe.func_def, f1, {
                'linkage': [], 'name': '$loop', 'blocks': [
                    {'label': '@start', 'phis': [], 'inst': [], 'jump': []},
                    {'label': '@loop', 'phis': [
                        {'var': '%x', 'type': 'w', 'cases': [
                            {'label': '@start', 'value': '100'},
                            {'label': '@loop', 'value': '%x1'}]
                         }],
                        'inst': [{'var': '%x1', 'type': 'w', 'op': 'sub', 'p1': '%x', 'p2': '1'}],
                        'jump': {'jump': 'jnz', 'test': '%x1', 'notzero': '@loop', 'zero': '@end'}},
                    {'label': '@end', 'phis': [], 'inst': [], 'jump': {'jump': 'ret'}}]}
            ),
        ]

        self.assertEqual(test_elements(tests), 0)


if __name__ == '__main__':
    unittest.main(failfast=True)
