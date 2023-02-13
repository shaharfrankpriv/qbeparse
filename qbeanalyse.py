#!/usr/bin/env python3
import argparse
import enum
import json
import os
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
        return cls.CONST, name


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
        return "(" + " ".join(map(lambda x: str(x), l)) + ")"


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
                self.const = int(self.val)

    def __str__(self):
        if self.vscope == VScope.CONST:
            return f"{self.const}"
        else:
            s = f"{self.vscope.value}{self.val}"
        if self.const is not None:
            s += f" ({self.const})"
        return s


class PhiCase(object):
    def __init__(self, label: str, value: Value):
        self.label = label
        self.value = value

    def __str__(self) -> str:
        return f"({self.label} -> {str(self.value)})"


class Phi(object):
    def __init__(self, var: Var, cases: List[PhiCase]):
        self.var = var
        self.cases = cases

    def __str__(self):
        s = f"{str(self.var)}"
        for c in self.cases:
            s += f" {str(c)}"
        return s


class JType(enum.Enum):
    JMP = "jmp"
    RET = "ret"
    JNZ = "jnz"
    HLT = "hlt"

    @classmethod
    def Name2Jtype(cls, name: str) -> 'JType':
        values = ["jmp", "ret", "jnz", "hlt"]
        if name in values:
            return JType(name)
        raise Exception(f"JType: bad type {name}")


class Jump(object):
    def __init__(self, jtype: JType, true: str | None = None, false: str | None = None,
                 value: Value | None = None):
        self.jtype = jtype
        # unconditional label or true (nonzero) label
        self.true = true[1:] if type(true) == str else None
        # conditional false label, else None
        self.false = false[1:] if type(false) == str else None
        self.value = value      # optional ret value, else None

    def __str__(self):
        s = f"{self.jtype.value}"
        if self.jtype == JType.HLT:
            return s
        elif self.jtype == JType.RET:
            return s + f" {self.value}" if self.value is not None else ""
        elif self.jtype == JType.JMP:
            return s + f" {self.true}"
        elif self.jtype == JType.JNZ:
            return s + f" {self.value} ? {self.true} : {self.false}"
        else:
            return "???"

    def Targets(self) -> List[str]:
        if self.jtype == JType.HLT:
            return ["Finish"]
        elif self.jtype == JType.RET:
            return []   # return to caller
        elif self.jtype == JType.JMP:
            return [f"{self.true}"]
        elif self.jtype == JType.JNZ:
            return [f"{self.true}", f"{self.false}"]
        else:
            return ["???"]


class Inst(object):
    def __init__(self, op: str, p1: Value, p2: Value | None = None, ret: Var | None = None) -> None:
        self.op = op
        self.p1 = p1
        self.p2 = p2
        self.ret = ret

    def __str__(self) -> str:
        s = "%-15s <-" % self.ret if self.ret else "%-18s" % ' '
        s += f" {self.op} "
        s += f"({self.p1}, {self.p2})" if self.p2 else f"({self.p1})"
        return s


class Call(object):
    def __init__(self, name: str, args: List[Var], ret: Var | None):
        self.name = name
        self.args = args
        self.ret = ret

    def __str__(self) -> str:
        s = f"call {self.name} {Var.ListStr(self.args)} -> {self.ret}\n"
        return s


class Block(object):
    def __init__(self, name: str, phis: List[Phi], instructions: List[Inst | Call], jump: Jump):
        self.label = name
        self.phis = phis
        self.jump = jump
        self.instructions = instructions

    def AddInst(self, inst: Inst):
        self.instructions.append(inst)

    def __str__(self) -> str:
        s = f"\tBlock {self.label}\n"
        for p in self.phis:
            s += '\t\tPhi ' + str(p) + '\n'
        for i in self.instructions:
            s += '\t\t' + str(i) + '\n'
        s += '\t\t' + str(self.jump) + '\n'
        return s

    def Dot(self, funcname: str, symbols: 'Symbols') -> str:
        name = self.label
        s = ""
        if self.jump.jtype == JType.RET:
            s += f'\t\t{name} -> "{funcname}_ret"\n'
        for t in self.jump.Targets():
            s += f"\t\t{name} -> {t}\n"
        for i in self.instructions:
            if type(i) == Call:
                fname = i.name[1:]
                if fname in symbols.functions:
                    s += f"\t\t{name} -> {fname}_Start[color=red]\n"
                    s += f"\t\t{fname}_ret -> {name}[color=blue]\n"
                else:
                    s += f"\t\t{name} -> {fname}[color=green]\n"
        return s


class Function(object):
    def __init__(self, name: str, params: List[Var], ret: VType | None):
        self.name = name
        self.params = params
        self.ret = ret
        self.blocks: List[Block] = []

    def AddBlock(self, block: Block):
        self.blocks.append(block)

    def __str__(self) -> str:
        s = f"Function: {self.name} {Var.ListStr(self.params)} -> {self.ret}\n"
        for b in self.blocks:
            s += str(b)
        return s

    def Dot(self, symbols: 'Symbols') -> str:
        s = f"\tsubgraph cluster_{self.name} {{\n"
        s += f'\t\tlabel = "{self.name}"\n'
        s += "\t\tstyle=filled;\n"
        s += f"\t\t{self.name}_Start [ style = rounded ];\n"
        s += f"\t\t{self.name}_Start -> {self.blocks[0].label};\n"
        # {Var.ListStr(self.params)} -> {self.ret}\n"
        for b in self.blocks:
            s += b.Dot(self.name, symbols)
        s += "\t}\n"
        return s


class Symbols(object):
    def __init__(self):
        self.data = []
        self.functions: Dict[str, Function] = {}
        self.exdata = []
        self.exfunc: Dict[str, Function] = {}

    def AddFunction(self, func: Function):
        self.functions[func.name] = func

    def AddData(self):
        pass


class Qbe(object):
    def __init__(self, filename: str, symbols: Symbols):
        self.file = filename
        self.verbose = False
        self.debug = False
        self.qbe_dict: dict = {}
        self.functions: Dict[str, Function] = {}
        self.symbols = symbols

    def SetVerbose(self, flag: bool = True):
        self.verbose = flag

    def SetDebug(self, flag: bool = True):
        self.debug = flag

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

    def ProcessPhi(self, func: Function, e: dict) -> Phi:
        self.Debug(f"[{func.name}] Phi: ", e)
        var: Var = Var(e["var"], e["type"])
        cases: List[PhiCase] = []
        for c in e["cases"]:
            cases.append(PhiCase(c["label"], c["value"]))
        return Phi(var, cases)

    def ProcessJump(self, func: Function, e: dict) -> Jump:
        self.Debug(f"{func.name} Jump: ", e)
        jtype = JType.Name2Jtype(e["jump"])
        if jtype == JType.HLT:
            return Jump(jtype)
        elif jtype == JType.RET:
            return Jump(jtype, value=e.get("ret_value", None))
        elif jtype == JType.JMP:
            return Jump(jtype, true=e["target"])
        elif jtype == JType.JNZ:
            return Jump(jtype, value=e["test"], true=e["notzero"], false=e["zero"])
        else:
            raise Exception(f"bad jtype {jtype}")

    def ProcessCall(self, e: dict) -> Call:
        op = e["op"]
        name = e["name"]
        ret = Var(e["ret_var"], e["ret_type"]) if "ret_var" in e else None
        args = self.ProcessParams(e.get("args", []))
        return Call(name, args, ret)

    def ProcessInstruction(self, func: Function, e: dict) -> Inst | Call:
        self.Debug(f"[{func.name}] Inst: ", e)
        op = e["op"]
        if op == "call":
            return self.ProcessCall(e)
        var = Var(e["var"], e["type"]) if "var" in e else None
        p1 = Value(e["p1"])
        p2 = Value(e["p2"]) if "p2" in e else None
        return Inst(op, p1, p2, var)

    def ProcessBlock(self, func: Function, e: dict, next: str | None) -> Block:
        label = e["label"][1:]
        phis: List[Phi] = []
        insts: List[Inst | Call] = []
        self.Verbose(f"ProcessBlock: [{func.name}] process {label}")
        for p in e["phis"]:
            phi = self.ProcessPhi(func, p)
            phis.append(phi)
            self.Verbose(f"ProcessBlock: [{func.name}] \tPhi: {str(phi)}")
        for i in e["inst"]:
            inst = self.ProcessInstruction(func, i)
            self.Verbose(f"ProcessBlock: [{func.name}] \tInst: {str(inst)}")
            insts.append(inst)
        # self.Debug(name, e)
        jdir = e.get("jump", {})
        if not jdir:
            if not next:
                raise Exception(f"missing ending jump on last block? {e}")
            jdir = dict(jump="jmp", target=next)
        jump = self.ProcessJump(func, jdir)
        self.Verbose(f"ProcessBlock: [{func.name}] \tJump: {str(jump)}")
        return Block(label, phis=phis, jump=jump, instructions=insts)

    def ProcessFunction(self, e: dict):
        name = f'{e["name"][1:]}'
        ret = e.get("return_type", None)
        params = self.ProcessParams(e.get("params", []))
        self.Verbose(
            f"ProcessFunction: process {name} {Var.ListStr(params)} -> {ret}")
        self.Verbose(
            f"ProcessFunction: \t")
        func = Function(name, params=params, ret=ret)
        self.functions[name] = func
        self.symbols.AddFunction(func)
        nblocks = len(e["blocks"])
        for bi in range(nblocks):
            b = e['blocks'][bi]
            next_label = None
            if bi + 1 < nblocks:
                next_label = e['blocks'][bi+1]["label"]
            func.AddBlock(self.ProcessBlock(func, b, next=next_label))

    def ProcessType(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        self.Verbose(f"ProcessType: process {name}")
        self.Debug(name, e)

    def ProcessData(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        self.Verbose(f"ProcessData: process {name}")
        self.Debug(name, e)

    def ProcessElem(self, e: dict):
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

    def ElememtsSummary(self) -> None:
        self.Verbose(f"ProcessFile: process file '{self.file}'")
        self.qbe_dict = qbe.ParseText(
            open(self.file, "r").read() + "\n").as_dict()
        for e in self.qbe_dict["elems"]:
            name = f'{e["elem"]} {e["name"]}'
            print(name)

    def Dot(self) -> str:
        name = os.path.basename(self.file)
        name = name.replace(".", "_")
        s = f"digraph {name} {{\n"
        # s += "\tcompound=true;\n"
        s += '\tnode [ shape = "Mrecord" ];\n'
        for f in self.functions.values():
            s += f.Dot(self.symbols)
        s += "\tStart [ style = rounded ];\n"
        s += "\tStart -> main_Start\n"
        s += "\tFinish [ style = rounded ];\n"
        s += "\tmain_ret -> Finish\n"
        s += "}\n"
        return s


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='qbeanalyse',
        description='Parse & Analyse qbe files.',
        epilog='')
    parser.add_argument('filename', metavar='Filename',
                        nargs='+',)           # positional argument
    parser.add_argument('-v', '--verbose', action='store_true',
                        help="trace processing flow.")  # on/off flag
    parser.add_argument('-g', '--dot', action='store_true',
                        help="plot dot graph.")  # on/off flag
    parser.add_argument('-d', '--debug', action='store_true',
                        help='show raw dict (after parsing).')  # on/off flag
    parser.add_argument('-e', '--elem', action='store_true',
                        help='only list elements.')  # on/off flag
    args = parser.parse_args()
    symbols = Symbols()
    for f in args.filename:
        try:
            q = Qbe(f, symbols=symbols)
            if args.elem:
                q.ElememtsSummary()
                continue
            if args.debug:
                q.SetDebug()
            if args.verbose:
                q.SetVerbose()
            q.ProcessFile()
            if args.dot:
                print(q.Dot())
        except ParseException as e:
            print(f"Processing '{f}' failed: {str(e)}", file=sys.stderr)
        except IOError as e:
            print(f"Can't access/read '{f}': {str(e)}", file=sys.stderr)
    print()
    sys.exit(0)
