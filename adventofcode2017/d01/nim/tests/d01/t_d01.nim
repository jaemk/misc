import d01
import unittest

suite "day1 tests":
  test "part 1":
    check part_1("1122") == 3
    check part_1("1111") == 4
    check part_1("1234") == 0
    check part_1("91212129") == 9

  test "part 2":
    check part_2("1212") == 6
    check part_2("1221") == 0
    check part_2("123425") == 4
    check part_2("123123") == 12
    check part_2("12131415") == 4

