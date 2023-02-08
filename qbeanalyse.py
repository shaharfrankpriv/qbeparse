#!/usr/bin/env python3
import argparse
import json
import sys

from pyparsing import ParserElement
from pyparsing.exceptions import ParseException

import qbeparse as qbe


class Qbe(object):
    verbose: bool
    debug: bool
    file: str
    elem_summary: bool
    qbe_dict: dict

    def __init__(self, filename: str, args: argparse.Namespace):
        self.file = filename
        self.verbose = args.verbose
        self.debug = args.debug
        self.elem_summary = args.elem
        self.qbe_dict = {}

    def Verbose(self, s: str):
        if self.verbose:
            print(s)

    def Debug(self, s: str, d: dict):
        if self.debug:
            print(f"\n## {s}:")
            json.dump(d, sys.stdout, indent=4, sort_keys=False)
            print()

    def ProcessFunction(self, e: dict):
        name = f'{e["elem"]} {e["name"]}'
        self.Verbose(f"ProcessFunction: process {name}")
        self.Debug(name, e)

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
