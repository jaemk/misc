#!/usr/bin/env python3

import sys
import unittest
from typing import List, Set, Tuple, Dict


def get_input() -> str:
    with open('../input.txt', 'r') as f:
        return f.read()


class Part1:
    @staticmethod
    def is_valid(input_: str) -> bool:
        seen = set() # type: Set[str]
        for part in input_.strip().split():
            if part in seen:
                return False
            seen.add(part)
        return True

    @classmethod
    def solve(cls, input_: str) -> int:
        return sum(1 if cls.is_valid(line) else 0
                        for line in input_.strip().split('\n'))


class Part2:
    @staticmethod
    def freq(s: str) -> Tuple[Tuple[str, int], ...]:
        chars = {} # type: Dict[str, int]
        for c in s:
            try:
                chars[c] += 1
            except:
                chars[c] = 1
        keys = list(chars.keys())
        keys.sort()
        return tuple((k, chars[k]) for k in keys)

    @classmethod
    def is_valid(cls, input_: str) -> bool:
        seen = set() # type: Set[Tuple[Tuple[str, int], ...]]
        for part in input_.strip().split():
            part_chars = cls.freq(part)
            if part_chars in seen:
                return False
            seen.add(part_chars)
        return True

    @classmethod
    def solve(cls, input_: str) -> int:
        return sum(1 if cls.is_valid(line) else 0
                        for line in input_.strip().split('\n'))


def run() -> None:
    input_ = get_input()
    print('Day4-Part1: {}'.format(Part1.solve(input_)))
    print('Day4-Part2: {}'.format(Part2.solve(input_)))


class TestExamples(unittest.TestCase):
    def test_part1(self) -> None:
        cases = [('aa bb cc dd ee', True), ('aa bb cc dd aa', False), ('aa bb cc dd aaa', True)]
        for input_, expected in cases:
            self.assertEqual(expected, Part1.is_valid(input_), "Input: {}, expected: {}".format(input_, expected))

    def test_part2(self) -> None:
        cases = [('abcde fghij', True),
                 ('abcde xyz ecdab', False),
                 ('a ab abc abd abf abj', True),
                 ('iiii oiii ooii oooi oooo', True),
                 ('oiii ioii iioi iiio', False),
                ]
        for i, e in cases:
            self.assertEqual(e, Part2.is_valid(i), "Input: {}, expected: {}".format(i, e))


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

