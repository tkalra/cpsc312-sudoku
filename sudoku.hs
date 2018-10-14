import Control.Monad

--Heyy!

display sudoku row column = do
	if  not (row < 1 || row > 9 || column < 1 || column > 9)
		then do
			if (sudoku row column == 0)
				then putStr(". ")
			else
				putStr ((show (sudoku row column))++" ")
			when (column == 3 || column == 6) (putStr ("| "))
			when (column == 9 && (row == 3 || row == 6)) (putStr("\n---------------------"))
			if column == 9
				then do
					putStr("\n")
					display sudoku (row+1) 1
			else
				display sudoku row (column+1)
	else return ()
