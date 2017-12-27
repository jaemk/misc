package finger

import (
	"errors"
	"strconv"
)

var fingerVals = [10]int{
	// left hand (pinky to thumb)
	10, 10, 10, 10, 50,
	// right hand (thumb to pinky)
	5, 1, 1, 1, 1,
}

// Raised fingers must be consecutive going from index->pinky
func isValid(vals []int) bool {
	// slice out left and right hands,
	// ignoring both thumbs (two middle values)
	left := vals[:4]
	right := vals[6:]

	size := len(left)
	for i := 1; i < size; i++ {
		// go backwards through left hand (index->pinky)
		if left[(size-1)-i]-left[(size-1)-i+1] > 0 {
			return false
		}
		// go forwards through right hand (index->pinky)
		if right[i]-right[i-1] > 0 {
			return false
		}
	}
	return true
}

func Count(fingers string) (int, error) {
	// convert finger string to ints
	var vals [10]int
	for i, n := range fingers {
		v, _ := strconv.Atoi(string(n))
		vals[i] = v
	}
	sum := 0
	if !isValid(vals[:]) {
		return sum, errors.New("invalid")
	} else {
		for i, n := range vals {
			if n == 1 {
				sum = sum + fingerVals[i]
			}
		}
	}
	return sum, nil
}
