package main

import "testing"

func TestPart1(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"1122", 3},
		{"1111", 4},
		{"1234", 0},
		{"91212129", 9},
	}

	for _, c := range cases {
		val, err := Part1(c.in)
		if err != nil {
			t.Error("Error: %q", err)
		}
		if val != c.want {
			t.Error("%q -- want: %q, got: %q", c.in, val, c.want)
		}
	}
}

func TestPart2(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"1212", 6},
		{"1221", 0},
		{"123425", 4},
		{"123123", 12},
		{"12131415", 4},
	}

	for _, c := range cases {
		val, err := Part2(c.in)
		if err != nil {
			t.Error("Error: %q", err)
		}
		if val != c.want {
			t.Error("%q -- want: %q, got: %q", c.in, val, c.want)
		}
	}
}
