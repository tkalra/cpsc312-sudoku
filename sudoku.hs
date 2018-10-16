import Control.Monad
import Data.Char
import Data.List

display sudoku row column = do
	if  not (row < 1 || row > 9 || column < 1 || column > 9)
		then do
			if (sudoku row column == 0)
				then putStr(". ")
			else
				putStr ((show (sudoku row column))++" ")
			when (column == 3 || column == 6) (putStr ("| "))
			when (column == 9 && (row == 3 || row == 6)) (putStr("\n---------------------"))
			if column == 9 then do
				putStr("\n")
				display sudoku (row+1) 1
			else
				display sudoku row (column+1)
	else return ()

get_file = do
	putStrLn("Enter the name of the file containing the sudoku:")
	file_name <- getLine
	file <- readFile file_name
	if length (filter isDigit file) >= 81 then return file
	else do
		putStrLn("File corrupt")
		file <- get_file
		return file

initialize = do
	file <- get_file
	let sudoku_str = [x | x <- file, isDigit x] in
		let sudoku row column = digitToInt (sudoku_str!!((row-1)*9+column-1)) in return sudoku

delete e l = [ x | x <- l, x /= e ]


--is_solution sudoku row column = do

make_row sudoku row 9 = [sudoku row 9]
make_row sudoku row column = (sudoku row column):make_row sudoku row (column+1)



verify_rows sudoku row =
	if row > 9 then True
	else do
		let lst = make_row sudoku row 1
		if sort lst /= [1..9] then False
		else verify_rows sudoku (row + 1)



make_column sudoku 9 column = [sudoku 9 column]
make_column sudoku row column = (sudoku row column):make_column sudoku (row+1) column


verify_columns sudoku column =
	if column > 9 then True
	else do
		let lst = make_column sudoku 1 column
		if sort lst /= [1..9] then False
		else verify_columns sudoku (column + 1)

make_square sudoku row column act_row act_col
	| act_row == row+2 && act_col == column+2 = [sudoku act_row act_col]
	| act_col == column+2 = sudoku act_row act_col : make_square sudoku row column (act_row+1) column
	| otherwise = sudoku act_row act_col : make_square sudoku row column act_row (act_col +1)


verify_squares sudoku row column =
	if row>7 then True
	else do
		let lst = make_square sudoku row column row column
		if sort lst /= [1..9] then False
		else
			if column == 7 then verify_squares sudoku (row+3) 1
			else verify_squares sudoku row (column+3)

is_solution sudoku = verify_rows sudoku 1 && verify_columns sudoku 1 && verify_squares sudoku 1 1



find_empty sudoku row column
	| row > 9 = (-1, -1)
	| column == 9 = if sudoku row column == 0 then (row, column)
		else find_empty sudoku (row+1) 1
	| otherwise = if sudoku row column == 0 then (row, column)
		else find_empty sudoku row (column+1)


find_playable sudoku row column = [ x | x <- [1..9],
	not (elem x (make_row sudoku row 1)),
	not (elem x (make_column sudoku 1 column)),
	not (elem x (make_square sudoku (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3)) (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3))))]
