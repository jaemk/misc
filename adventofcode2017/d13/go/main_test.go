package main

import "testing"

var TEST_INPUT = `
0: 3
1: 2
4: 4
6: 4
`

func TestPart1(t *testing.T) {
	val, err := Part1(TEST_INPUT)
	if err != nil {
		t.Errorf("Error: %q", err)
	}
	if val != 24 {
		t.Errorf("Part1 -- want: %d, got: %d", 24, val)
	}
}

func TestPart2(t *testing.T) {
	val, err := Part2(TEST_INPUT)
	if err != nil {
		t.Errorf("Error: %q", err)
	}
	if val != 10 {
		t.Errorf("Part2 -- want: %d, got: %d", 10, val)
	}
}
