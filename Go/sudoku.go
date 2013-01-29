package main

import (
	"io/ioutil"
	// "bufio"
	// "bytes"
	"fmt"
	// "io"
	// "os"
	"errors"
	"strconv"
	"strings"
)

type Sudoku struct {
	matrix        [][]int
	solved        bool
	solutionCount int
}

func NewSudoku(path string) (*Sudoku, error) {
	s := new(Sudoku)
	var err error
	s.matrix, err = readMatrix(path)
	if err != nil {
		panic(err)
	}

	return s, nil
}

func (s *Sudoku) GetSolutionsCount() {
	return s.solutionCount
}

func (s *Sudoku) registerSolution() {
	s.solutionCount++
	s.PrintMatrix()
	if !s.solved {
		s.solved = true
	}
}

func (s *Sudoku) isSolved() bool {
	return s.solved
}

func (s *Sudoku) Solve() {
	s.bruteforcePosition(0, 0)
}

func readMatrix(path string) ([][]int, error) {
	content, err := ioutil.ReadFile(path)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(content), "\r")

	if len(lines) != 9 {
		return nil, errors.New("Insufficient number of ROWS")
	}

	matrix := make([][]int, 9, 9)
	for i, line := range lines {
		//fmt.Printf("%v: %v\n", i, line)

		stringRows := strings.Split(line, " ")

		integerRow := make([]int, 9, 9)
		for j, str := range stringRows {

			val, err := strconv.Atoi(str)
			if err != nil {
				return nil, err
			}
			integerRow[j] = val
		}
		matrix[i] = integerRow
	}
	return matrix, nil
}

// Verify that 'val' can be legally placed at (row,col)
func (s *Sudoku) ValidValueAtPosition(row, col, val int) bool {
	if s.ValidInSquare(row, col, val) &&
		s.ValidInColumnAndRow(row, col, val) {
		// validInRow(row, val, matrix) {
		return true
	}

	return false
}

func (s *Sudoku) ValidInSquare(row, col, val int) bool {
	row, col = int(row/3)*3, int(col/3)*3

	for i := row; i < row+3; i++ {
		for j := col; j < col+3; j++ {
			//fmt.Printf("row, col = %v, %v\n", i, j)
			if s.matrix[i][j] == val {
				return false
			}
		}
	}
	return true
}

func (s *Sudoku) ValidInColumnAndRow(row, col, val int) bool {
	for i := 0; i < 9; i++ {
		if s.matrix[row][i] == val ||
			s.matrix[i][col] == val {
			return false
		}
	}
	return true
}

func (s *Sudoku) bruteforcePosition(row, col int) {
	// we use '0' to indicate a non-filled block
	if s.matrix[row][col] == 0 {
		for i := 1; i < 10; i++ {
			if s.ValidValueAtPosition(row, col, i) {
				// place the value and attempt to solve
				s.matrix[row][col] = i
				// attempt to solve the sudoku with placed value
				s.nextPosition(row, col)
				// clean up after attempt
				s.matrix[row][col] = 0
			}
		}
	} else {
		s.nextPosition(row, col)
	}
}

func (s *Sudoku) nextPosition(row, col int) {
	// we run through the matrix row by row
	// meaning we only change rows when we're in
	// the final column
	if col < 8 {
		s.bruteforcePosition(row, col+1)
	} else {
		// if we're in the final collumn in the final 
		// row; we have a solution
		// - else we iterate to next row and reset the collumn
		if row < 8 {
			s.bruteforcePosition(row+1, 0)
		} else {
			s.registerSolution()
		}
	}
}

func (s *Sudoku) PrintMatrix() {
	for _, row := range s.matrix {
		fmt.Println(row)
	}
	fmt.Println("")
}

func main() {

	sudo, err := NewSudoku("solvable88.txt")

	if err != nil {
		panic(err)
	}
	// matrix := readMatrix()

	sudo.Solve()
	fmt.Printf("\nsolutions: %v", sudo.GetSolutionsCount())
}
