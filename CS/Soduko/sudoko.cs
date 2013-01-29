/*
 ======================================================================================
 Name        : suduko.cs
 Author      : Bjarke Frøsig
 Version     : 0.02
 Copyright   : GPL
 Description : Sudoku solver ported from significantly faster C code, by Bjarke Frøsig
 Command-Line: "soduko < soduko1.txt"
 Result      : List of possible solutions.
 ======================================================================================
*/

using System;

namespace Soduko
{
    static class scanf_emu // C# doesn't have a scanf() function, so this less than nice class emulates the required functionality.
    {
        static string[] lineTokens = new string[0];
        static int elementpos = 0;

        static string GetToken()
        {
            if (elementpos == lineTokens.Length)
            {
                lineTokens = Console.ReadLine().Split(" ".ToCharArray());
                elementpos = 0;
            }

            return lineTokens[elementpos++].Trim();
        }

        public static int GetNumber()
        {
            string token;
            while (String.IsNullOrEmpty(token = GetToken())) ;

            return int.Parse(token);
        }
    }

    class sudoko
    {
        const int BOARD_HEIGHT = 9;
        const int BOARD_WIDTH = 9;
        const int MAX_POSITION = (BOARD_HEIGHT * BOARD_WIDTH);

        const int DUMMY_NUMBER = 0;
        const int MIN_NUMBER = 1;
        const int NUMBERS = 9;

        private class Board
        {
            public int[,] board;

            public Board()
            {
                board = new int[BOARD_HEIGHT, BOARD_WIDTH];
            }
        }

        private static void print_board(Board b)
        {
	        for(int i = 0; i < BOARD_HEIGHT; ++i)
	        {
		        for(int j = 0; j < BOARD_WIDTH; ++j)
			        Console.Write(b.board[i, j].ToString() + ' ');
		        Console.WriteLine();
	        }
            Console.WriteLine();
        }

        private static int board_row(int position)
        {
            return ((position) / BOARD_WIDTH);
        }

        private static int board_column(int position)
        {
            return ((position) % BOARD_WIDTH);
        }

        private static int board_box_top(int pos)
        {
            return (board_row(pos) < 3 ? 0 : (board_row(pos) < 6 ? 3 : 6));
        }

        private static int board_box_left(int pos)
        {
            return (board_column(pos) < 3 ? 0 : (board_column(pos) < 6 ? 3 : 6));
        }

        private static bool check_for_number(Board board, int position, int number)
        {
	        for(int i = 0; i < BOARD_WIDTH; ++i)
		        if(board.board[board_row(position), i] == number)
			        return false;

	        for(int i = 0; i < BOARD_HEIGHT; ++i)
		        if(board.board[i, board_column(position)] == number)
			        return false;

	        int box_left = board_box_left(position), box_top = board_box_top(position);

	        for(int x = box_left; x < box_left + 3; ++x)
		        for(int y = box_top; y < box_top + 3; ++y)
			        if(board.board[y, x] == number)
				        return false;

	        return true;
        }

        private static void update_board(Board board, int position, int number)
        {
	        board.board[board_row(position), board_column(position)] = number;
        }

        private static bool board_position_assigned(Board board, int position)
        {
	        return board.board[board_row(position), board_column(position)] != DUMMY_NUMBER;
        }

        private static void evaluate_board_setup(Board board, int position)
        {
            if (position == MAX_POSITION)
            {
                print_board(board);
                return;
            }

	        if(board_position_assigned(board, position))
		        evaluate_board_setup(board, position + 1);
	        else
	        {
		        for(int number = MIN_NUMBER; number < (MIN_NUMBER + NUMBERS); ++number)
			        if(check_for_number(board, position, number))
			        {
				        update_board(board, position, number);
				        evaluate_board_setup(board, position + 1);
			        }

		        update_board(board, position, DUMMY_NUMBER);
	        }
        }

        private static void setup_board(Board board)
        {
	        for(int x = 0; x < BOARD_WIDTH; ++x)
		        for(int y = 0; y < BOARD_HEIGHT; ++y)
			        board.board[y, x] = DUMMY_NUMBER;
        }

        private static void read_board_from_console(Board board)
        {
	        for(int pos = 0; pos < MAX_POSITION; ++pos)
                board.board[board_row(pos), board_column(pos)] = scanf_emu.GetNumber();
        }

        static void Main(string[] args)
        {
	        Board board = new Board();

	        read_board_from_console(board);

	        evaluate_board_setup(board, 0);
        }
    }
}
