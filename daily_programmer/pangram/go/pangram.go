package pangram

import (
	"strings"
)

const testVersion = 1

var Alpha = map[rune]bool{}

func alphabet() map[rune]bool {
	for i := 97; i < 97+26; i++ {
		Alpha[rune(i)] = false
	}
	return Alpha
}

func IsPangram(s string) bool {
	alpha := alphabet()
	for _, c := range strings.ToLower(s) {
		alpha[c] = true
	}

	ok := true
	for _, v := range alpha {
		if !v {
			ok = false
			break
		}
	}
	return ok
}
