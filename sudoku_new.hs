import Control.Monad
import Data.Char
import Data.List

{-
One must load the file in ghci and type run
Another possibility is to compile this file using ghc and run the executable, which will solve the same sudokus in a lesser time




In general we structure a sudoku by a list of 81 digits such that the first 9 are the first column, the second 9 are
the second column, etc... the firsts of every column are the first row, the seconds of every column are the second row, etc...
We represent empty cells by 0.
-}


--To display a sudoku, we start at the first element of the list and go through it by printing all elements
--(replacing 0 by .) and printing some special characters sometimes, in order to differentiate the lines, columns, squares...
display sudoku n = do
	when (n /= 0 && n `mod` 9 == 0) (putStr("|\n"))
	when (n `mod` 27 == 0) (putStrLn("-------------------------"))
	when (n /= 81 && n `mod` 3 == 0) (putStr ("| "))
	if  n < 81 then do
		if sudoku!!n == 0 then
			putStr(". ")
		else
			putStr ((show (sudoku!!n)) ++ " ")
		display sudoku (n+1)
	else return ()

--This function asks for a file name, reads it, and if it has a sudoku format it returns the sudoku in a string == [Char]
--Sudoku file format is simply a file containing at least 81 integers. We will neglect any character that is not a digit
--and keep the first 81 digits, putting them in the same order in the sudoku string that they were wrote in.
--Which means the first 9 are the first column, the second 9 the second column, etc...
get_file = do
	putStrLn("Enter the name of the file containing the sudoku:")
	file_name <- getLine
	file <- readFile file_name
	if length (filter isDigit file) >= 81 then return file
	else do
		putStrLn("File corrupt")
		return ""

--This function will call get_file to get a sudoku from a file, and convert it from [Char] to [Int]
initialize = do
	file <- get_file
	let sudoku = [ (digitToInt x) | x <- file, isDigit x] in
		return sudoku


--If we want to get an element from a sudoku based its row and column,
--this function will get that element from a sudoku string
get_value sudoku (row, column) =  (sudoku!!((row-1)*9+column-1))

--This function has been taken from stack overflow (which doesn't mean we couldn't write it ourselves, we were looking for library functions on the net)
--What it does is replace the nth element from a list by an element of our choice.
--This function is going to be useful to create new sudoku boards
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

--Because the last function replaces the nth element from a board, and it is easier for us to visualize the board
--as a 9x9 matrix, then this function will take the row and column of the element to change and call the previous
--function giving it the position of that element in the list
replace_sudoku (row, column) newVal sudoku = replaceNth ((row-1)*9+column-1) newVal sudoku

--In order to know what digits cannot be played, and in order to know if a sudoku is solved, we need to
--know what elements are in each row, column and square. The functions below will do just that.

--This function takes a certain row and returns a list containing the elements of that row
make_row sudoku row 9 = [sudoku!!((row - 1)*9 + 8)]
make_row sudoku row column = (sudoku!!((row - 1)*9 + column-1)):(make_row sudoku row (column+1))

--This function will verify if the current row contains all of 1..9 and call itself on the next row, until the 9th row has been tested
verify_rows sudoku row =
	if row > 9 then True
	else sort (make_row sudoku row 1) == [1..9] && verify_rows sudoku (row + 1)

--This function takes a certain column and returns a list containing the elements of that column
make_column sudoku 9 column = [sudoku!!(8*9 + column - 1)]
make_column sudoku row column = (sudoku!!((row -1)*9 + column - 1)):make_column sudoku (row+1) column

--This function will verify if the current column contains all of 1..9 and call itself on the next column, until the 9th column has been tested
verify_columns sudoku column =
	if column > 9 then True
	else sort (make_column sudoku 1 column) == [1..9] && verify_columns sudoku (column + 1)

--This function takes the starting point of a square as 'row' and 'column' for which it will generate in a list all its elements.
--The parameters 'act_row' and 'act_col' are the 'actual_row' and 'actual column', meaning they will be used to
--keep track of which elements have been visited yet. When we first call the function they should be the same as 'row' and 'column'
make_square sudoku row column act_row act_col
	| act_row == row+2 && act_col == column+2 = [get_value sudoku (act_row, act_col)]
	| act_col == column+2 = get_value sudoku (act_row, act_col):make_square sudoku row column (act_row+1) column
	| otherwise = get_value sudoku (act_row, act_col):make_square sudoku row column act_row (act_col +1)

--This function verifies if a given square (given by starting point) contains 1..9 and calls itself on the next, until the 9th square has been tested
verify_squares sudoku row column =
	if row>7 then True
	else
		if column == 7 then sort (make_square sudoku row column row column) == [1..9] && verify_squares sudoku (row +3) 1
		else sort (make_square sudoku row column row column) == [1..9] && verify_squares sudoku row (column +3)

--This function will use all the previous verify functions (calling them with 1st row, 1st column and 1st square) to verify if a sudoku is a solution or not
is_solution sudoku = verify_rows sudoku 1 && verify_columns sudoku 1 && verify_squares sudoku 1 1

--This function finds the first empty cell in a sudoku and returns its row and column in a tuple
find_empty sudoku row column
	| row > 9 = (-1, -1)
	| column == 9 = if get_value sudoku (row, column) == 0 then (row, column)
		else find_empty sudoku (row+1) 1
	| otherwise = if get_value sudoku (row, column) == 0 then (row, column)
		else find_empty sudoku row (column+1)

--This function takes a tuple of row and column and returns a list of all playable digits in this cell
--that is a list of all elements from 1 to 9 that if inserted in this cell will not break any sudoku rule
find_playable sudoku (row, column) = [ x | x <- [1..9],
	not (elem x (make_row sudoku row 1)),
	not (elem x (make_column sudoku 1 column)),
	not (elem x (make_square sudoku (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3)) (row - ((row-1) `mod` 3)) (column - ((column-1) `mod` 3))))]

--The solving function, the heart of the project, takes just a sudoku and returns a tuple of Bool and sudoku ([Int]).
--If the sudoku given to it is full, it returns its correctness and the sudoku itself.
--otherwise it creates a list of possible next state sudokus by playing valid digits in the next empty cell,
--creates a list of outputs of itself applied on every element of that list, filters this new list to keep
--the ones that returned True in the tuple, and returns the head of this filtered list.
--Notice that we added the element (False, []) to the end of the result of the filternig function,
--in case the filtered list is empty we can still use head.
solve sudoku
	| find_empty sudoku 1 1 == (-1, -1) = (is_solution sudoku, sudoku)
	| otherwise = head ((filter (\ (bool, string) -> bool) 
		[ solve new_sudoku | new_sudoku <- [ replace_sudoku (find_empty sudoku 1 1) element sudoku | 
		element <- (find_playable sudoku (find_empty sudoku 1 1)) ] ] )++[(False, [])])

has_duplicate [] = False
has_duplicate (h:t) = (h /= 0 && elem h t) || has_duplicate t

--Sometimes the sudoku is trivially impossible to solve. Such cases arise if the sudoku given is already breaking the rules.
--These functions will be useful in finding such sudokus

--This function verifies whether or not a certain row is breaking the rules, and calls itself on the next row, until the 9th has been tested
verify_rows_impossible sudoku row =
	if row > 9 then False
	else has_duplicate (make_row sudoku row 1) || verify_rows_impossible sudoku (row + 1)

--This function verifies whether or not a certain column is breaking the rules, and calls itself on the next column, until the 9th has been tested
verify_columns_impossible sudoku column =
	if column > 9 then False
	else has_duplicate (make_column sudoku 1 column) || verify_columns_impossible sudoku (column + 1)

--This function verifies whether or not a certain square is breaking the rules, and calls itself on the next square, until the 9th has been tested
verify_squares_impossible sudoku row column =
	if row>7 then False
	else
		if column == 7 then has_duplicate (make_square sudoku row column row column) || verify_squares_impossible sudoku (row +3) 1
		else has_duplicate (make_square sudoku row column row column) || verify_squares_impossible sudoku row (column +3)

--This function uses the 3 previous ones to find whether any row, column or square is breaking the rules
is_trivially_impossible sudoku = verify_rows_impossible sudoku 1 || verify_columns_impossible sudoku 1 || verify_squares_impossible sudoku 1 1

--The function run will take care of initializing a sudoku, verifying if it is a sudoku, if it is trivially impossible
--to solve, and if not then it will solve it and print it
run = do
	sudoku <- initialize
	display sudoku 0
	if sudoku == [] then do return ()
	else
		if is_trivially_impossible sudoku then do
			putStrLn ("Sudoku trivially impossible")
			return ()
		else do
			let (bool, solved) = solve sudoku
			putStrLn("The solution is")
			display solved 0
			return ()

--The function below serves the ability to compile the code into an executable
main = run
