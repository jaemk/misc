package main

import "os"
import "fmt"
import "io/ioutil"
import "strings"
import "strconv"

func readInput() (string, error) {
	b, err := ioutil.ReadFile("../input.txt")
	if err != nil {
		fmt.Println(err)
	}
	return string(b), nil
}

func parseInput(s string) ([]int, error) {
	str := strings.TrimSpace(s)
	runes := []rune(str)
	nums := make([]int, len(runes))
	for i, r := range runes {
		n, err := strconv.Atoi(string(r))
		if err != nil {
			return nil, err
		}
		nums[i] = n
	}
	return nums, nil
}

func Part1(in string) (int, error) {
	input, err := parseInput(in)
	if err != nil {
		return 0, err
	}
	offset := make([]int, len(input))
	copy(offset, input)

	first, offset := offset[0], offset[1:]
	offset = append(offset, first)

	sum := 0
	for i, _ := range input {
		if input[i] == offset[i] {
			sum += input[i]
		}
	}
	return sum, nil
}

func halfAround(i int, half int) int {
	total := half * 2
	ind := i + half
	for {
		if ind < total {
			return ind
		}
		ind -= total
	}
}

func Part2(in string) (int, error) {
	input, err := parseInput(in)
	if err != nil {
		return 0, err
	}
	half := len(input) / 2
	sum := 0
	for i, v := range input {
		ind := halfAround(i, half)
		if v == input[ind] {
			sum += v
		}
	}
	return sum, nil
}

func fatal(e error) {
	fmt.Println(e)
	os.Exit(1)
}

func main() {
	input, err := readInput()
	if err != nil {
		fatal(err)
	}

	p1, err := Part1(input)
	if err != nil {
		fatal(err)
	}
	fmt.Printf("d1-p1: %d\n", p1)

	p2, err := Part2(input)
	if err != nil {
		fatal(err)
	}
	fmt.Printf("d1-p2: %d\n", p2)
}
