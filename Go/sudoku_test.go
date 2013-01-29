package sudoku

import "testing"

const solvable88 string = `0 7 0 0 0 0 0 8 0
0 3 0 7 6 2 0 0 1
0 0 1 9 8 0 0 0 0
1 0 0 0 0 0 0 0 0
8 0 3 0 0 0 0 0 2
0 0 6 0 0 0 0 0 8
0 0 0 0 3 1 6 0 0
5 0 0 2 4 9 0 1 0
0 1 0 0 0 0 0 9 0`

const hard string = `0 0 0 0 0 0 0 0 0
0 0 1 0 0 3 0 0 0
8 0 0 0 0 0 0 0 6
0 0 0 2 0 9 0 1 0
6 4 0 0 0 0 9 0 0
0 0 0 0 0 0 0 0 0
0 0 2 0 0 0 7 3 0
7 0 0 6 4 0 0 0 0
0 0 0 8 0 0 0 0 0`

const noSolution string = `7 0 8 0 0 0 7 0 0
0 0 0 0 6 0 0 5 0
0 0 0 9 0 0 0 2 4
1 6 7 0 0 8 0 0 5
0 0 0 6 3 0 9 0 0
9 3 0 7 1 4 2 0 0
8 0 0 1 5 2 4 6 3
5 0 6 0 4 9 8 1 7
3 1 4 8 7 0 5 9 2`

func TestSolve88(t *testing.T) {
	// load sudoku
	s, err := NewSudokuFromString(solvable88)
	if err != nil {
		t.Error(err)
	}
	s.Solve()

	if s.GetSolutionsCount() != 88 {
		t.Errorf("Expected 88 != Actual %v", s.GetSolutionsCount())
	}
}

func (s *Sudoku) Reset() {
	s.solutionCount = 0
}

func BenchmarkSolveHard(b *testing.B) {
	b.StopTimer()
	// load sudoku

	s, err := NewSudokuFromString(hard)
	if err != nil {
		b.Error(err)
	}
	b.StartTimer()

	for i := 0; i < b.N; i++ {
		s.Reset()
		s.Solve()
	}
}

func BenchmarkSolveFail(b *testing.B) {
	b.StopTimer()
	// load sudoku

	s, err := NewSudokuFromString(noSolution)
	if err != nil {
		b.Error(err)
	}
	b.StartTimer()

	for i := 0; i < b.N; i++ {
		s.Reset()
		s.Solve()
	}
}
