#!/usr/bin/env python3

import sys
import unittest
from typing import List, Dict, Tuple
from math import sqrt, ceil


def get_input() -> int:
    return 265149


class Part1:
    class EdgeWalk:
        def __init__(self, bound: int, start: int) -> None:
            self.bound = bound
            self.x = bound
            self.y = -bound + 1
            self.num = start
            super().__init__()

        def walk_to(self, n: int) -> None:
            while self.num < n:
                if self.x == self.bound:
                    if self.y < self.bound:
                        self.y += 1
                    else:
                        self.x -= 1
                elif self.y == self.bound:
                    if self.x > -self.bound:
                        self.x -= 1
                    else:
                        self.y -= 1
                elif self.x == -self.bound:
                    if self.y > -self.bound:
                        self.y -= 1
                    else:
                        self.x += 1
                elif self.y == -self.bound:
                    if self.x < self.bound:
                        self.x += 1
                    else:
                        raise Exception("Unreachable. Stepped around entire edge.")
                self.num += 1

        def dist(self) -> int:
            return abs(self.x) + abs(self.y)

    @classmethod
    def solve(cls, n: int) -> int:
        if n == 1: return 0
        if n < 10:
            if n % 2 == 0: return 1
            else: return 2
        else:
            # field size must be odd
            size = ceil(sqrt(n))
            size = size if size % 2 != 0 else size + 1
            # +/- bounding edges of field
            bound = (size - 1) // 2
            num_start = ((size - 2) ** 2) + 1
            walker = cls.EdgeWalk(bound, num_start)
            walker.walk_to(n)
            return walker.dist()


class Part2:
    class EdgeWalk:
        def __init__(self) -> None:
            self.x = self.y = self.step_count = 0
            self.field = {} # type: Dict[Tuple[int, int], int]
            self.direction = 'right'

        def left_is_empty(self) -> bool:
            if self.direction == 'right':
                left = (self.x, self.y + 1)
            elif self.direction == 'up':
                left = (self.x - 1, self.y)
            elif self.direction == 'left':
                left = (self.x, self.y - 1)
            elif self.direction == 'down':
                left = (self.x + 1, self.y)
            return self.field.get(left, None) is None

        def turn_left(self) -> None:
            if self.direction == 'right':
                self.direction = 'up'
            elif self.direction == 'up':
                self.direction = 'left'
            elif self.direction == 'left':
                self.direction = 'down'
            elif self.direction == 'down':
                self.direction = 'right'

        def step_forward(self) -> None:
            if self.direction == 'right':
                self.x += 1
            elif self.direction == 'up':
                self.y += 1
            elif self.direction == 'left':
                self.x -= 1
            elif self.direction == 'down':
                self.y -= 1

        def count_surrounding(self) -> int:
            surrounding = [
                (self.x - 1, self.y),
                (self.x + 1, self.y),
                (self.x, self.y - 1),
                (self.x, self.y + 1),
                (self.x - 1, self.y - 1),
                (self.x - 1, self.y + 1),
                (self.x + 1, self.y - 1),
                (self.x + 1, self.y + 1),
            ]
            return sum(self.field.get(pos, 0) for pos in surrounding)

        def step(self) -> int:
            if self.step_count == 0:
                self.step_count += 1
                self.field[(self.x, self.y)] = 1
                return 1
            if self.step_count == 1:
                self.step_count += 1
                self.x += 1
                self.field[(self.x, self.y)] = 1
                return 1

            if self.left_is_empty():
                self.turn_left()

            self.step_forward()
            n = self.count_surrounding()
            self.field[(self.x, self.y)] = n
            return n

    @classmethod
    def test_solve(cls, order_no: int) -> int:
        walker = cls.EdgeWalk()
        n = 0
        for _ in range(order_no):
            n = walker.step()
        return n

    @classmethod
    def solve(cls, sentinel: int) -> int:
        walker = cls.EdgeWalk()
        while True:
            n = walker.step()
            if n > sentinel:
                return n


def run() -> None:
    input_ = get_input()
    print('Day2-Part1: {}'.format(Part1.solve(input_)))
    print('Day2-Part2: {}'.format(Part2.solve(input_)))


class TestExamples(unittest.TestCase):
    def test_part1(self) -> None:
        cases = [(1, 0), (12, 3), (23, 2), (1024, 31)]
        for input_, expected in cases:
            self.assertEqual(expected, Part1.solve(input_), "Input: {}, expected: {}".format(input_, expected))

    def test_part2(self) -> None:
        cases = [(1, 1), (2, 1), (3, 2), (4, 4), (5, 5)]
        for i, e in cases:
            self.assertEqual(e, Part2.test_solve(i), "Input: {}, expected: {}".format(i, e))


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

