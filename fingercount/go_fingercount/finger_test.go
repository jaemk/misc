package finger

import (
	"testing"
)

var cases = []struct {
	input   string
	want    int
	message string
}{
	{"0111011100", 37, ""},
	{"1010010000", 0, "invalid"},
	{"0011101110", 73, "invalid"},
	{"0000110000", 55, ""},
	{"1111110001", 0, "invalid"},
}

func TestCount(t *testing.T) {
	for _, n := range cases {
		out, err := Count(n.input)
		if err != nil && err.Error() != n.message {
			t.Fatalf("Count(%s) = %s, wanted %s", n.input, err, n.message)
		} else if out != n.want {
			t.Fatalf("Count(%s) = %d, wanted %d", n.input, out, n.want)
		}
	}
	t.Log(len(cases), "test cases")
}

func BenchmarkCount(b *testing.B) {
	for i := 0; i < b.N; i++ {
		for _, n := range cases {
			Count(n.input)
		}
	}
}
