#!/usr/bin/python3

import sys
import unittest


def get_input():
    with open('../input.txt', 'r') as f:
        return f.read()


def part1(input_):
    a = input_.strip()
    b = a[1:] + a[0]
    return sum(int(a) if a == b else 0 for a, b in zip(a, b))


def part2(input_):
    s = input_.strip()
    size = len(s)
    half = size // 2
    sum_ = 0
    for i in range(0, size):
        j = i + half
        if j > size - 1:
            j -= size
        if s[i] == s[j]:
            sum_ += int(s[i])
    return sum_


def run():
    input_ = get_input()
    print('Day1-Part1: {}'.format(part1(input_)))
    print('Day1-Part2: {}'.format(part2(input_)))


class TestExamples(unittest.TestCase):
    def test_part1(self):
        cases = [('1122', 3), ('1111', 4), ('1234', 0), ('91212129', 9)]
        for input_, expected in cases:
            self.assertEqual(expected, part1(input_), "Input: {}, expected: {}".format(input_, expected))

    def test_part2(self):
        cases = [('1212', 6), ('1221', 0), ('123425', 4), ('123123', 12), ('12131415',4)]
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

