#!/usr/bin/env python3
import argparse
import enum
import json
import sys

from pyparsing import ParserElement
from pyparsing.exceptions import ParseException
from typing import List, Dict, Tuple
import qbeparse as qbe


class VScope(enum.Enum):
    GLOBAL = '$'
    TEMP = '%'
    LABEL = '@'
    USER = ':'
    CONST = ''

    @classmethod
    def ParseName(cls, name: str) -> Tuple['VScope', str]:
        scopes = [cls.GLOBAL, cls.TEMP, cls.LABEL, cls.USER]
        if name[0] in map(lambda x: x.value, scopes):
            return VScope(name[0]), name[1:]
        return cls.CONST, name[1:]


class VType(enum.Enum):
    WORD = 'w'
    LONG = 'l'
    SINGLE = 's'
    DOUBLE = 'd'
    BYTE = 'b'
    HALF = 'h'
    VAR = ""

    @classmethod
    def ParseType(cls, name: str) -> 'VType':
        if name in map(lambda x: x.value, (cls.WORD, cls.LONG, cls.SINGLE, cls.DOUBLE, cls.BYTE, cls.HALF)):
            return VType(name)
        if name == "...":
            return VType.VAR
        raise Exception(f"unkown vtype {name}")


class Var(object):
    def __init__(self, name: str, vtype: VType):
        self.vtype = vtype
        self.vscope, self.name = VScope.ParseName(name)

    def __str__(self):
        return f"({self.vtype} {str(self.vscope.value)}{self.name})"

    @classmethod
    def ListStr(cls, l: List['Var']) -> str:
        return " ".join(map(lambda x: str(x), l))


class Value(object):
    def __init__(self, val: str):
        self.vscope, self.val = VScope.ParseName(val)
        # this asserts as the parser should not let it pass
        assert self.vscope not in [VScope.LABEL, VScope.USER]
        self.const: None | float | int = None
        if self.vscope == VScope.CONST:
            if self.val[:2] in ['_s', '_d']:
                self.const = float(self.val[2:])
            else:
                self.const = int(self.val[2:])


class PhiCases(object):
    def __init__(self, label: str, value: Value):
        self.label = label
        self.value = Value


class Phi(object):
    def __init__(self, var: Var, cases: List[PhiCases]):
        self.var = var
        self.cases = cases


class JType(enum.Enum):
    JMP = "jmp"
    RET = "ret"
    JNZ = "jnz"
    HLT = "hlt"


class Jump(object):
    def __init__(self, true: str | None, false: str | None,
                 jtype: JType, ret: Value | None = None):
        self.jtype = jtype
        self.true = true    # unconditional label or true (nonzero) label
        self.false = false  # conditional false label, else None
        self.ret = ret      # optional ret value, else None


class Block(object):
    def __init__(self, name: str, phis: List[Phi], jump: Jump):
        self.label = name
        self.phis = phis
        self.jump = jump


class Function(object):
    def __init__(self, name: str, params: List[Var], ret: VType | None):
        self.name = name
        self.params = params
        self.ret = ret
        self.blocks: List[Block] = []

    def AddBlock(self, block: Block):
        self.blocks.append(block)


class Qbe(object):
    def __init__(self, filename: str, args: argparse.Namespace):
        self.file = filename
        self.verbose = args.verbose
        self.debug = args.debug
        self.elem_summary = args.elem
        self.qbe_dict: dict = {}
        self.functions: List[Function] = []

    def Verbose(self, s: str):
        if self.verbose:
            print(s)

    def Debug(self, s: str, d: dict):
        if self.debug:
            print(f"\n## {s}:")
            json.dump(d, sys.stdout, indent=4, sort_keys=False)
            print()

    def ProcessParams(self, params: List) -> List[Var]:
        out: List[Var] = []
        for p in params:
            assert type(p) == list
            assert len(p) >= 1 and len(p) <= 2
            if len(p) == 2:
                out += [Var(p[1], p[0])]
            else:
                out += [Var(p[0], p[0])]
        return out

    def ProcessBlock(self, blocks: List):
        # self.Verbose(f"ProcessBlock: process {name}")
        # self.Debug(name, e)
        return

    def ProcessFunction(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        ret = e.get("return_type", None)
        params = self.ProcessParams(e["params"])
        self.Verbose(f"ProcessFunction: process {name}")

        self.Verbose(
            f"ProcessFunction: \tParams: {Var.ListStr(params)} -> {ret}")
        self.Debug(name, e)
        for b in e["blocks"]:
            self.ProcessBlock(b)

    def ProcessType(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        self.Verbose(f"ProcessType: process {name}")
        self.Debug(name, e)

    def ProcessData(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        self.Verbose(f"ProcessData: process {name}")
        self.Debug(name, e)

    def ProcessElem(self, e: dict):
        if self.elem_summary:
            name = f'{e["elem"]} {e["name"]}'
            print(name)
            return
        elem = e['elem']
        if elem == qbe.ELEM_FUNC:
            self.ProcessFunction(e)
        elif elem == qbe.ELEM_TYPE:
            self.ProcessType(e)
        elif elem == qbe.ELEM_DATA:
            self.ProcessData(e)
        else:
            raise Exception(f"Internal: bad elem type {elem}")

    def ProcessFile(self) -> None:
        self.Verbose(f"ProcessFile: process file '{self.file}'")
        self.qbe_dict = qbe.ParseText(
            open(self.file, "r").read() + "\n").as_dict()
        for e in self.qbe_dict["elems"]:
            self.ProcessElem(e)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='qbeanalyse',
        description='Parse & Analyse qbe files.',
        epilog='')
    parser.add_argument('filename', metavar='Filename',
                        nargs='+',)           # positional argument
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="trace processing flow.")  # on/off flag
    parser.add_argument('-d', '--debug', action='store_true',
                        help='show raw dict (after parsing).')  # on/off flag
    parser.add_argument('-e', '--elem', action='store_true',
                        help='only list elements.')  # on/off flag
    args = parser.parse_args()
    for f in args.filename:
        try:
            q = Qbe(f, args)
            q.ProcessFile()
        except ParseException as e:
            print(f"Processing '{f}' failed: {str(e)}", file=sys.stderr)
        except IOError as e:
            print(f"Can't access/read '{f}': {str(e)}", file=sys.stderr)
    print()
    sys.exit(0)
