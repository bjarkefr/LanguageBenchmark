package main

import (
	"flag"
	"fmt"
	"github.com/paddie/godoku"
)

var path string
var doPrint bool

func init() {
	flag.StringVar(&path, "path", "", "'-path <path-to-sudoku>'")
	flag.BoolVar(&doPrint, "print", false, "'-print' to print solutions")
}

func main() {
	flag.Parse()

	if path == "" {
		panic("No path argument. -help to get help")
	}

	fmt.Println(path)

	s, err := godoku.NewSudokuFromFile(path, 9)

	if err != nil {
		fmt.Printf("path = '%v'", path)
		panic(err)
	}
	if doPrint {
		s.SolveAndPrint()
	} else {
		s.Solve()
	}
}
