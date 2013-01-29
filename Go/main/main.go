package main

import (
	"flag"
	"fmt"
	"goduko"
)

var path string
var doPrint bool

func init() {
	flag.StringVar(&path, "path", "tmp", "'-path <path-to-sudoku>'")
	flag.BoolVar(&doPrint, "print", false, "'-print' to print solutions")
}

func main() {
	flag.Parse()

	fmt.Println(path)

	s, err := goduko.NewSudokuFromFile(path, doPrint)

	if err != nil {
		fmt.Printf("path = '%v'", path)
		panic(err)
	}

	s.Solve()

	fmt.Printf("isSolved = %v\n", s.IsSolved())

}
