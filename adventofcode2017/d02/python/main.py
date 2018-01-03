#!/usr/bin/python3

import sys
import unittest


def get_input():
    with open('../input.txt', 'r') as f:
        return f.read()


class Part1:
    @classmethod
    def row_check(_, row):
        elems = [int(e) for e in row.strip().split()]
        min_ = elems[0]
        max_ = elems[0]
        for elem in elems[1:]:
            if elem < min_:
                min_ = elem
            elif elem > max_:
                max_ = elem
        return max_ - min_

    @classmethod
    def checksum(cls, input_):
        return sum(cls.row_check(row) for row in input_.strip().split('\n'))


class Part2:
    @classmethod
    def row_check(_, row):
        elems = [int(e) for e in row.strip().split()]
        for i, a in enumerate(elems[:-1]):
            for b in elems[i+1:]:
                if a == b:
                    continue
                if a % b == 0:
                    return a // b
                if b % a == 0:
                    return b // a
        raise Exception("Unreachable")

    @classmethod
    def checksum(cls, input_):
        return sum(cls.row_check(row) for row in input_.strip().split('\n'))


def part1(input_):
    return Part1.checksum(input_)


def part2(input_):
    return Part2.checksum(input_)


def run():
    input_ = get_input()
    print('Day2-Part1: {}'.format(part1(input_)))
    print('Day2-Part2: {}'.format(part2(input_)))


class TestExamples(unittest.TestCase):
    def test_part1(self):
        cases = [('5 1 9 5', 8), ('7 5 3', 4), ('2 4 6 8', 6)]
        for input_, expected in cases:
            self.assertEqual(expected, part1(input_), "Input: {}, expected: {}".format(input_, expected))

    def test_part2(self):
        cases = [('5 9 2 8', 4), ('9 4 7 3', 3), ('3 8 6 5', 2)]
        for i, e in cases:
            self.assertEqual(e, part2(i), "Input: {}, expected: {}".format(i, e))


def main(args):
    if args and args[0] == 'test':
        suite = unittest.TestSuite()
        suite.addTest(unittest.makeSuite(TestExamples))
        runner = unittest.TextTestRunner()
        runner.run(suite)
    else:
        run()


if __name__ == '__main__':
    main(sys.argv[1:])

