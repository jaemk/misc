package main

import "fmt"
import "os"
import "io/ioutil"
import "strings"
import "strconv"

func readInput() (string, error) {
	b, err := ioutil.ReadFile("../input.txt")
	if err != nil {
		return "", err
	}
	return string(b), nil
}

type FireWall struct {
	scanners []ScanLayer
}

func parseFireWall(input string) (*FireWall, error) {
	scanners := make([]ScanLayer, 5)
	for _, line := range strings.Split(strings.TrimSpace(input), "\n") {
		parts := strings.Split(line, ":")
		depth, err := strconv.ParseUint(strings.TrimSpace(parts[0]), 10, 32)
		if err != nil {
			return nil, err
		}
		range_, err := strconv.ParseUint(strings.TrimSpace(parts[1]), 10, 32)
		if err != nil {
			return nil, err
		}
		scanners = append(scanners, ScanLayer{depth: depth, range_: range_, startPos: 0})
	}
	return &FireWall{scanners: scanners}, nil
}

type ScanLayer struct {
	depth    uint64
	range_   uint64
	startPos uint64
}

func (self *ScanLayer) IsAtZeroAtTime(picoseconds uint64) bool {
	stepsInDir := self.range_ - 1
	stepMultipleAtZero := stepsInDir * 2
	return (picoseconds+self.startPos)%stepMultipleAtZero == 0
}

func (self *ScanLayer) Severity() uint64 {
	return self.depth * self.range_
}

func Part1(input string) (uint64, error) {
	wall, err := parseFireWall(input)
	if err != nil {
		return 0, err
	}
	severity := uint64(0)
	for _, scan := range wall.scanners {
		if scan.IsAtZeroAtTime(scan.depth) {
			severity += scan.Severity()
		}
	}
	return severity, nil
}

func Part2(input string) (uint64, error) {
	wall, err := parseFireWall(input)
	if err != nil {
		return 0, nil
	}
	delay := uint64(0)
	for {
		clearPath := true
		for _, scan := range wall.scanners {
			if scan.IsAtZeroAtTime(delay + scan.depth) {
				clearPath = false
				break
			}
		}
		if clearPath {
			break
		} else {
			delay += 1
		}
	}
	return delay, nil
}

func run() error {
	input, err := readInput()
	if err != nil {
		return err
	}

	p1, err := Part1(input)
	if err != nil {
		return err
	}
	fmt.Printf("d13-p1: %d\n", p1)

	p2, err := Part2(input)
	if err != nil {
		return err
	}
	fmt.Printf("d13-p2: %d\n", p2)
	return nil
}

func main() {
	if err := run(); err != nil {
		fmt.Println(os.Stderr, err)
		os.Exit(1)
	}
}
