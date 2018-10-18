import Control.Monad
import Data.Char
import Data.List

display sudoku n = do
	when (n `mod` 9 == 0) (putStr("|\n"))
	when (n `mod` 27 == 0) (putStrLn("------------------------"))
	when (n `mod` 3 == 0) (putStr ("| "))
	if  n < 81 then do
		if sudoku!!n == 0 then
			putStr(". ")
		else
			putStr ((show (sudoku!!n))++" ")
		display sudoku (n+1)
	else return ()

get_file = do
	putStrLn("Enter the name of the file containing the sudoku:")
	file_name <- getLine
	file <- readFile file_name
	if length (filter isDigit file) >= 81 then return file
	else do
		putStrLn("File corrupt")
		return ""

initialize = do
	file <- get_file
	let sudoku_str = [ (digitToInt x) | x <- file, isDigit x] in
		return sudoku_str

get_value sudoku (row, column) =  (sudoku!!((row-1)*9+column-1))

replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

replace_sudoku (row, column) newVal sudoku = replaceNth ((row-1)*9+column-1) newVal sudoku

make_row_str sudoku row 9 = [sudoku!!((row - 1)*9 + 8)]
make_row_str sudoku row column = (sudoku!!((row - 1)*9 + column-1)):(make_row_str sudoku row (column+1))

verify_rows_str sudoku row =
	if row > 9 then True
	else sort (make_row_str sudoku row 1) == [1..9] && verify_rows_str sudoku (row + 1)

make_column_str sudoku 9 column = [sudoku!!(8*9 + column - 1)]
make_column_str sudoku row column = (sudoku!!((row -1)*9 + column - 1)):make_column_str sudoku (row+1) column

verify_columns_str sudoku column =
	if column > 9 then True
	else sort (make_column_str sudoku 1 column) == [1..9] && verify_columns_str sudoku (column + 1)

make_square_str sudoku row column act_row act_col
	| act_row == row+2 && act_col == column+2 = [get_value sudoku (act_row, act_col)]
	| act_col == column+2 = get_value sudoku (act_row, act_col):make_square_str sudoku row column (act_row+1) column
	| otherwise = get_value sudoku (act_row, act_col):make_square_str sudoku row column act_row (act_col +1)

verify_squares_str sudoku row column =
	if row>7 then True
	else
		if column == 7 then sort (make_square_str sudoku row column row column) == [1..9] && verify_squares_str sudoku (row +3) 1
		else sort (make_square_str sudoku row column row column) == [1..9] && verify_squares_str sudoku row (column +3)


is_solution_str sudoku = verify_rows_str sudoku 1 && verify_columns_str sudoku 1 && verify_squares_str sudoku 1 1

find_empty_str sudoku row column
	| row > 9 = (-1, -1)
	| column == 9 = if get_value sudoku (row, column) == 0 then (row, column)
		else find_empty_str sudoku (row+1) 1
	| otherwise = if get_value sudoku (row, column) == 0 then (row, column)
		else find_empty_str sudoku row (column+1)

find_playable_str sudoku (row, column) = [ x | x <- [1..9],
	not (elem x (make_row_str sudoku row 1)),
	not (elem x (make_column_str sudoku 1 column)),
	not (elem x (make_square_str sudoku (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3)) (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3))))]


solve_str sudoku
	| find_empty_str sudoku 1 1 == (-1, -1) = (is_solution_str sudoku, sudoku)
	| find_playable_str sudoku (find_empty_str sudoku 1 1) == [] = (False, [])
	| otherwise = head ((filter (\ (bool, string) -> bool) [ solve_str new_sudoku | new_sudoku <- [ replace_sudoku (find_empty_str sudoku 1 1) element sudoku | element <- (find_playable_str sudoku (find_empty_str sudoku 1 1)) ] ] )++[(False, [])])
