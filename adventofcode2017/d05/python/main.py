#!/usr/bin/env python3

import sys
import unittest
from typing import List, Set, Tuple, Dict


def get_input() -> str:
    with open('../input.txt', 'r') as f:
        return f.read()


class Part1:
    @classmethod
    def solve(cls, input_: str) -> int:
        instrs = [int(line) for line in input_.strip().split()]
        ind = 0
        steps = 0
        while 0 <= ind < len(instrs):
            val = instrs[ind]
            instrs[ind] += 1
            ind += val
            steps += 1
        return steps


class Part2:
    @classmethod
    def solve(cls, input_: str) -> int:
        instrs = [int(line) for line in input_.strip().split()]
        ind = 0
        steps = 0
        while 0 <= ind < len(instrs):
            val = instrs[ind]
            if val >= 3:
                instrs[ind] -= 1
            else:
                instrs[ind] += 1
            ind += val
            steps += 1
        return steps


def run() -> None:
    input_ = get_input()
    print('Day5-Part1: {}'.format(Part1.solve(input_)))
    print('Day5-Part2: {}'.format(Part2.solve(input_)))


class TestExamples(unittest.TestCase):
    test_in = '0\n 3\n0\n1\n-3\n'
    def test_part1(self) -> None:
        expect = 5
        self.assertEqual(expect, Part1.solve(self.test_in), "Input: {}, expected: {}".format(self.test_in, expect))

    def test_part2(self) -> None:
        expect = 10
        self.assertEqual(expect, Part2.solve(self.test_in), "Input: {}, expected: {}".format(self.test_in, expect))


def main(args: List[str]) -> None:
    if args and args[0] == 'test':
        suite = unittest.TestSuite()
        suite.addTest(unittest.makeSuite(TestExamples)) # type: ignore
        runner = unittest.TextTestRunner()
        res = runner.run(suite)
        if res.errors or res.failures:
            sys.exit(1)
    else:
        run()


if __name__ == '__main__':
    main(sys.argv[1:])

