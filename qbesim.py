#!/usr/bin/env python3

import argparse
import sys
from pyparsing import ParserElement
from pyparsing.exceptions import ParseException
from typing import List, Dict, Tuple, Type, TypeVar
import qbeparse as qbe
from qbeanalyse import *


class Const(enum.Enum):
    NO_ASID = 0
    NO_FNID = 0
    NO_OP = ""


class Message:
    pass

MessageType = TypeVar("MessageType", bound="Message")

class Frame(Message):
    def __init__(self, op: str, asid: int, funcid: int):
        self.op = op
        self.asid = asid
        self.funcid = funcid
        self.param1 = 0
        self.value1: Value | None = None
        self.param2 = 0
        self.value2: Value | None = None

    def SetParam1(self, param: int, value: Value | None = None):
        self.param1 = param
        self.value1 = value

    def SetParam2(self, param: int, value: Value | None = None):
        self.param2 = param
        self.value2 = value

    def Update(self, param: int, value: Value):
        if self.param1 == param:
            self.value1 = value
        if self.param2 == param:
            self.value2 = value


class Result(Message):
    def __init__(self, asid: int, funcid: int, param: int, value: Value | None = None):
        self.asid = asid
        self.funcid = funcid
        self.param = param
        self.value = value


class Port(object):
    def __init__(self, l1: 'Logic' | None = None, l2: 'Logic' | None = None):
        self.l1 = l1
        self.l2 = l2

    def Connect(self, logic: 'Logic'):
        if self.l1 is None:
            self.l1 = logic
        elif self.l2 is None:
            self.l2 = logic
        else:
            raise Exception(
                f"Port already connected to {self.l1} and {self.l2}")

    def Other(self, logic: 'Logic') -> 'Logic' | None:
        if self.l1 == logic:
            return self.l2
        elif self.l2 == logic:
            return self.l1
        else:
            raise Exception(f"Port {self} not connected to {logic}")

    def __str__(self) -> str:
        return f"{self.l1} <-> {self.l2}"


class Logic(object):
    def __init__(self, name: str, inp: Port, outp: Port):
        self.name = name
        self.inp = inp
        self.outp = outp
        self.out : Message | None = None

    def Tick(self):
        raise Exception("unimplemented tick function")

    def Out(self) -> MessageType | None:
        return None

    def __str__(self) -> str:
        return f"[{self.name}]"


class FramePort(Port):
    def Read(self, dest: Logic) -> Frame | None:
        other = self.Other(dest)
        if other is None:
            return None
        return other.Out()


class ResultPort(Port):
    def Read(self, dest: Logic) -> Result | None:
        other = self.Other(dest)
        if other is None:
            return None
        return other.Out()


class EU(Logic):
    def __init__(self, name: str, inp: FramePort, outp: ResultPort):
        Logic.__init__(self, name, inp, outp)
        self.out: Result | None = None

    def Enqueue(self, frame: Frame):
        pass


class EUInt(EU):
    ops = {
        'add': lambda p1, p2: p1 + p2,
        'sub': lambda p1, p2: p1 - p2,
        'div': lambda p1, p2: int(p1 / p2),
        'mul': lambda p1, p2: p1 * p2,
        'neg': lambda p1, p2: ~p1,
        'udiv': lambda p1, p2: int(((p1+2**32) if p1 < 0 else p1) / ((p2+2**32) if p2 < 0 else p2)),
        'rem': lambda p1, p2: p1 % p2,
        'urem': lambda p1, p2: int(((p1+2**32) if p1 < 0 else p1) % ((p2+2**32) if p2 < 0 else p2)),
        'or': lambda p1, p2: p1 | p2,
        'xor': lambda p1, p2: p1 ^ p2,
        'and': lambda p1, p2: p1 & p2,
        'sar': lambda p1, p2: p1 >> p2,
        'shr': lambda p1, p2: int(((p1+2**32) if p1 < 0 else p1) >> ((p2+2**32) if p2 < 0 else p2)),
        'shl': lambda p1, p2: p1 << p2,
    }
    def __init__(self, name: str, inp: FramePort, outp: ResultPort):
        EU.__init__(self, name, inp, outp)

    def Tick(self):
        frame = self.inp.Read()
        if frame is None:
            self.out = None
            return
        if frame.op in self.ops:
            self.out = self.ops[frame.op](frame.p1.GetInt(), frame.p2.GetInt())

        

class FrameBuffer(object):
    def __init__(self, name: str, nframes: int):
        self.name = name
        self.nframes = nframes
        self.frames: List[Frame] = []
        self.free_frames: List[Frame] = []
        for n in range(nframes):
            self.free_frames.append(
                Frame(Const.NO_OP.value, Const.NO_ASID.value, Const.NO_FNID.value))


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
