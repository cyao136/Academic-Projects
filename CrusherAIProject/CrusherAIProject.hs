-- project1.hs

-- Cheng Yu Yao 
-- 36210128
-- e5k8

-- Dongfang Amon Ge
-- 20021127
-- y8n8


-- TABLE OF CONTENTS

-- 0. Import Modules
-- 1. Main Crusher Functions
-- 2. Move Generation
-- 3. Static Board Evaluation
-- 4. Board Rotation
-- 5. Other Functions
-- 6. Test Cases

import Data.List -- for transpose, maximumBy
import Data.Ord -- for comparing

-- 1. Main Crusher Functions

-- INPUT:
-- [String] - list of boards (history of game)
-- Char 	- current player
-- Int 		- depth to look ahead ( > 0)
-- Int 		- n (dimension of board)

-- OUTPUT:
-- [String] - new list of boards (best move cons'ed to history)

-- returns the best move cons'ed to the history list of boards
crusher_y8n8 :: [String] -> Char -> Int -> Int -> [String]
crusher_y8n8 lob c depth n = best_next_board_y8n8 (minimax_y8n8 c c depth n (tail lob) (head lob)) : lob

-- INPUT:
-- (Int, [String]) - duple returned by minimax
	-- Int 			- propagated score
	-- [String] 	- propagated path of best moves

-- OUTPUT:
-- String - the next best move (2nd in the path)

-- given the duple returned by minimax, retrieves the best next move from the propagated path of moves
best_next_board_y8n8 :: (Int, [String]) -> String
best_next_board_y8n8 (_, (x:[])) = []-- if there's only one move in the path, just return empty (no legal move)

best_next_board_y8n8 (_, xs) = -- second in the path
 head $ drop 1 xs


-- INPUT:
-- Char - maximizing player
-- Char - current player
-- Int - depth to look ahead 
-- Int - n (dimension of board)
-- [String] - list of boards (previous history)
-- String - board (current board)

-- OUTPUT:
-- (Int, [String]) - duple of score and list of boards on the "best path" propagated

-- minimax search on a board to return a duple of the best score and path of best moves
minimax_y8n8 :: Char -> Char -> Int -> Int -> [String] -> String -> (Int, [String]) 

-- base case: no legal moves from a node -- score win_score depending on player
-- NOTE: this comes before the depth 0 base case because our static evaluator does not check no_legal_move 
-- 		it doesn't check no_legal_move because that would require generating new boards (not allowed for static evaluator)
-- 		thus, the evaluator doesn't know to score a winning score for a no legal move state, so it's done here instead
minimax_y8n8 _ c_p _ n lob board 
	| no_legal_move_y8n8
			(board_to_rows_y8n8 n board) 
			c_p 
			n 
			(map (board_to_rows_y8n8 n) lob) = ((win_score_y8n8 c_p), [board])  

-- base case: depth 0 -- evaluate the board statically
minimax_y8n8 _ c_p 0 n lob board = ((board_evaluator_y8n8 (board_to_rows_y8n8 n board) 
												c_p
												n 
												(map (board_to_rows_y8n8 n) lob)), 
									[board])  

-- recursive case 1: current player is the player to maximize
minimax_y8n8 max_p c_p depth n lob board
	| max_p == c_p	= 
		(fst duple, board:(snd duple)) -- append current board to the list of boards in the propagated duple
	where duple = -- the propagated duple
		(maximumBy (comparing fst) 		-- MAXimize scores
			  (map (minimax_y8n8		  			-- recurse: map minimax onto children states:
						max_p					 	-- maximizing player stays the same
						(swap_player_y8n8 c_p)   	-- current player gets swapped
						(depth - 1)         		-- depth is decremented
						n							-- dimensions stay the same
						(board:lob))				-- current board added to the history path
			  	  (state_gen_y8n8 (board_to_rows_y8n8 n board) -- generate children
						 c_p
						 n
						 (map (board_to_rows_y8n8 n) lob))
			   )
		)

-- recursive case 2: current player is not the player to maximize
-- the reason we overload here instead of just using two guards is because 'where' isn't allowed in between guards
minimax_y8n8 max_p c_p depth n lob board	
	| max_p /= c_p 	=
		(fst duple, board:(snd duple)) -- append current board to the list of boards in the propagated duple
	where duple = -- the propagated duple
		(minimumBy (comparing fst) 	-- MINimize scores
				(map (minimax_y8n8		-- recurse: map minimax onto children states:
						max_p				-- maximizing player stays the same
						(swap_player_y8n8 c_p)	-- current player gets swapped
						(depth - 1)			-- depth is decremented
						n					-- dimensions stay the same
						(board:lob))		-- current board added to the history path
					(state_gen_y8n8 (board_to_rows_y8n8 n board) -- children
						 c_p
						 n
						 (map (board_to_rows_y8n8 n) lob))
				)
		)


-- 2. Move Generation

-- INPUT:
-- [String] - board in a multiple lines format
-- Char - current player
-- Int - dimension of the board
-- [[String]] - list of boards already travelled

-- OUTPUT: 
-- [String] - list of boards in single string format

--removes the duplicates and empty board from lines_gen_y8n8 of the three rotated boards
--returns a list of next possible boards for the given board
state_gen_y8n8 :: [String] -> Char -> Int -> [[String]] -> [String]
state_gen_y8n8 bd c n path =
	(map rows_to_board_y8n8
	(remove_duplicates_y8n8 ((lines_gen_y8n8 bd c (2*n-1))++
				(map rotate_ccw_y8n8 (lines_gen_y8n8 (rotate_cw_y8n8 bd) c (2*n-1)))++
				(map rotate_cw_y8n8 (lines_gen_y8n8 (rotate_ccw_y8n8 bd) c (2*n-1)))) (bd:path)
	))

-- INPUT:
-- [String] - board in a multiple lines format
-- Char - current player
-- Int - number of lines in the board (dimension*2 - 1)

-- OUTPUT: list of boards

-- for every line in the given board, it generates the all the possible moves for each line
-- and returns each as a board
lines_gen_y8n8 :: [String] -> Char -> Int -> [[String]]
lines_gen_y8n8 bd c n
	| n == 0		= []
	| otherwise		= (line_gen_y8n8 bd c n) ++ (lines_gen_y8n8 bd c (n-1))

-- INPUT:
-- [String] - board in a multiple lines format
-- Char - current player
-- Int - index of the line in the board

-- OUTPUT: list of boards

--generate the next possible moves on a single line and recombining that line back into a full board
line_gen_y8n8 :: [String] -> Char -> Int -> [[String]]
line_gen_y8n8 bd c n =
	(fill_line_to_board_y8n8 bd n (move_left_y8n8 (head (drop (n-1) bd)) c 0))++
	(fill_line_to_board_y8n8 bd n (move_right_y8n8 (head (drop (n-1) bd)) c 0))++
	(fill_line_to_board_y8n8 bd n (leap_left_y8n8 (head (drop (n-1) bd)) c 0))++
	(fill_line_to_board_y8n8 bd n (leap_right_y8n8 (head (drop (n-1) bd)) c 0))

-- INPUT:
-- [String] - board in a multiple lines format
-- Int - index of the line in the board
-- [String] - list of different moves for that line

-- OUTPUT: list of boards

--returns boards from a list of generated lines
fill_line_to_board_y8n8 :: [String] -> Int -> [String] -> [[String]]
fill_line_to_board_y8n8 bd n lol
	| null lol		= []
	| otherwise		= ((take (n-1) bd)++((head lol):(drop n bd))):(fill_line_to_board_y8n8 bd n (tail lol))
	
-- Possible Moves:

-- (applies to move_left_y8n8, move_right_y8n8, leap_left_y8n8, and leap_right_y8n8)
-- INPUT:
-- String - line to perform the move
-- Char - current player
-- Int - accumulator for each checked block

-- OUTPUT: list of possible _____ (depend on function) moves for that line

-- returns a list of lines which has pieces moved to the _____ by 1 if possible
move_left_y8n8 :: String -> Char -> Int -> [String]
move_left_y8n8 line c acc
	|null (drop (acc+1) line)	= []
	|otherwise					= (((take acc line)++(move_left_helper_y8n8 (drop acc line) c)):
									(move_left_y8n8 line c (acc+1)))
	
-- (applies to move_left_helper_y8n8, move_right_helper_y8n8, leap_left_helper_y8n8, and leap_right_helper_y8n8)
-- INPUT:
-- String - line to perform the move
-- Char - current player

-- OUTPUT: line with the performed move

-- performs the _____ move and returns the line
move_left_helper_y8n8 :: String -> Char -> String
move_left_helper_y8n8 line c
	|((head line) == '-')
		&&	((head (tail line)) == c)		= (c:"-")++(drop 2 line)
	| otherwise								= line

move_right_y8n8 :: String -> Char -> Int -> [String]
move_right_y8n8 line c acc
	|null (drop (acc+1) line)	= []
	|otherwise					= (((take acc line)++(move_right_helper_y8n8 (drop acc line) c)):
									(move_right_y8n8 line c (acc+1)))
								
move_right_helper_y8n8 :: String -> Char -> String
move_right_helper_y8n8 line c
	|((head line) == c)
		&&	((head (tail line)) == '-')		= ('-':c:"")++(drop 2 line)
	| otherwise								= line

leap_left_y8n8 :: String -> Char -> Int -> [String]
leap_left_y8n8 line c acc
	|null (drop (acc+2) line)	= []
	|otherwise					= (((take acc line)++(leap_left_helper_y8n8 (drop acc line) c)):
									(leap_left_y8n8 line c (acc+1)))

leap_left_helper_y8n8 :: String -> Char -> String
leap_left_helper_y8n8 line c
	|((head line) /= c)
		&& ((head (tail line)) == c)
		&& ((head (tail (tail line))) == c)	= (c:c:"-")++(drop 3 line)
	| otherwise								= line								
								
leap_right_y8n8 :: String -> Char -> Int -> [String]
leap_right_y8n8 line c acc
	|null (drop (acc+2) line)	= []
	|otherwise					= (((take acc line)++(leap_right_helper_y8n8 (drop acc line) c)):
									(leap_right_y8n8 line c (acc+1)))

leap_right_helper_y8n8 :: String -> Char -> String
leap_right_helper_y8n8 line c
	|((head line) == c)
		&& ((head (tail line)) == c)
		&& ((head (tail (tail line))) /= c)	= ('-':c:c:"")++(drop 3 line)
	| otherwise								= line	
	
	
-- 3. Static Board Evaluation

-- INPUT: 
-- [String] - board in multiple lines format
-- Char - current player
-- Int - dimension of the board
-- [[String]] - traversed boards

-- OUTPUT: the score associated with the board
	
-- evaluates a board and returns its score
-- score < 0 : W advantage
-- score > 0 : B advantage
board_evaluator_y8n8 :: [String] -> Char -> Int -> [[String]] -> Int
board_evaluator_y8n8 bd c n path
	|(count_pieces_y8n8 bd 'W') < n  = win_score_y8n8 'B' -- B wins because W has less than n pieces left
	|(count_pieces_y8n8 bd 'B') < n	= win_score_y8n8 'W' -- vice versa
	|otherwise                  = 	3 * (count_pieces_y8n8 bd 'B')   -- piece advantage (weight 3)
									- 3 * (count_pieces_y8n8 bd 'W') 
									+ (unity_score_y8n8 bd 'B')			 -- unity advantage (how close your pieces are together)
									- (unity_score_y8n8 bd 'W')


-- INPUT:
-- [String] - board
-- Char - current player

-- OUTPUT: 
-- a score dependent on how unified your pieces are (adjacent pieces get +1)
unity_score_y8n8:: [String] -> Char -> Int
unity_score_y8n8 bd c =
	(unity_board_score_y8n8 bd c) +
	(unity_board_score_y8n8 (rotate_cw_y8n8 bd) c) +
	(unity_board_score_y8n8 (rotate_ccw_y8n8 bd) c)

-- INPUT:
-- [String] - board
-- Char - current player

-- OUTPUT: 
-- a score dependent on how unified your pieces are (adjacent pieces get +1)
unity_board_score_y8n8:: [String] -> Char -> Int
unity_board_score_y8n8 bd c
	|null bd					= 0
	|otherwise					= (unity_line_score_y8n8 (head bd) c 0) + 
									(unity_board_score_y8n8 (tail bd) c)

-- INPUT:
-- String - row
-- Char - current player
-- Int - accumulator score

-- OUTPUT: 
-- a score dependent on how unified your pieces are (adjacent pieces get +1)
unity_line_score_y8n8:: String -> Char -> Int -> Int
unity_line_score_y8n8 line c acc
	|null (tail line)			= acc
	|((head line) == c)&&
	 ((head (tail line)) == c)	= unity_line_score_y8n8 (tail line) c (acc+1)
	|otherwise					= unity_line_score_y8n8 (tail line) c acc
	

-- INPUT:
-- Char - player who wins

-- OUTPUT:
-- Int - score of that board

-- returns score of a won game, given the winning player
win_score_y8n8 :: Char -> Int
win_score_y8n8 'B' = 999
win_score_y8n8 'W' = -999
	
-- INPUT: 
-- [String] - board
-- Char - current player

-- OUTPUT: the number of pieces the current player has on the board
	
-- evaluates the number of pieces a player has on the board
count_pieces_y8n8 :: [String] -> Char -> Int
count_pieces_y8n8 bd c
	|null bd					= 0
	|otherwise					= (count_pieces_line_y8n8 (head bd) c)+(count_pieces_y8n8 (tail bd) c)

-- INPUT: 
-- String - line
-- Char - current player

-- OUTPUT: the number of pieces the current player has on the line
	
-- evaluates the number of pieces a player has on the line
count_pieces_line_y8n8 :: String -> Char -> Int
count_pieces_line_y8n8 line c
	|null line					= 0
	|(head line) == c			= 1 + (count_pieces_line_y8n8 (tail line) c)
	|otherwise					= count_pieces_line_y8n8 (tail line) c


-- 4. Board Rotation	
	
-- Input:
-- [String] - a board in multiple lines format

-- Output:
-- [String] - the board rotated CCW by one edge (60 deg)

-- rotate a board CCW
rotate_ccw_y8n8 :: [String] -> [String]
rotate_ccw_y8n8 [] = []
rotate_ccw_y8n8 board = 
	reverse $ 				-- flip the ORDER of the rows around
	unpad_y8n8 $   			-- get rid of padding *
	transpose $ 			-- transpose the matrix
	pad_y8n8 board (-1) 	-- pad the rows with * such that the \\\ rows line up vertically

-- Input:
-- [String] - a board in multiple lines format

-- Output:
-- [String] - the board rotated CW by one edge (60 deg)

-- rotate a board CW
rotate_cw_y8n8 :: [String] -> [String]
rotate_cw_y8n8 [] = []
rotate_cw_y8n8 board = 	
	map reverse $ 		-- flip EACH row around
	unpad_y8n8 $ 		-- get rid of padding *
	transpose $ 		-- transpose the matrix
	pad_y8n8 board 1 	-- pad the rows with * such that the /// rows line up vertically

-- Input:
-- [String] - a board in multiple lines format
-- Int 		- direction to rotate (1: CW, -1: CCW)

-- Output:
-- [String] - the board rotated CW by one edge (60 deg)

-- pad the rows with * symbols so that either the /// or \\\ rows line up vertically
-- each row becomes N + 2 long
pad_y8n8 :: [String] -> Int -> [String]
pad_y8n8 [] _ = []
pad_y8n8 (x:xs) d = pad_helper_y8n8 (x:xs) (length x - 1) d -- passes in how many * to pad the first row + direction


-- Input:
-- [String] - a board in multiple lines format
-- Int		- how many * we need to pad the first row
-- Int 		- direction to rotate (1: CW, -1: CCW)

-- Output:
-- [String] - the board rotated CW by one edge (60 deg)

-- pad the rows with * symbols so that either the /// or \\\ rows line up vertically
-- each row becomes N + 2 long
pad_helper_y8n8 :: [String] -> Int -> Int -> [String] 
pad_helper_y8n8 [] _ _ = []
pad_helper_y8n8 (x:xs) n d 
	| d == 1  = (pad_row_y8n8 x n) : pad_helper_y8n8 xs (n - 1) d 	-- CW, start padding on the right
	| d == -1 = (pad_row_y8n8 x (-n)) : pad_helper_y8n8 xs (n - 1) d 	-- CCW, start padding on the left

-- Input:
-- String   - a row of the board
-- Int		- abs: how many * we need to pad the first row
--			- sign: + pad on the right side
--					- pad on the left side
-- Output:
-- String - padded row (length N + 2)

-- pads a single row with n number of *'s
-- when n is > 0, pad with * on the right side; < 0, pad with * on the left side
pad_row_y8n8 :: String -> Int -> String 
pad_row_y8n8 xs n 
	| (n > 0)	= xs ++ pad_string 
	| (n < 0) 	= pad_string ++ xs
	| otherwise = xs  				-- n = 0 (longest row), don't pad
	where pad_string = replicate (abs n) '*' -- pad string: replicate |n| number of *

-- Input:
-- [String] - padded board (all rows length N + 2)

-- Output:
-- [String] - unpadded board (* removed)

-- unpads a board by removing *
unpad_y8n8 :: [String] -> [String] 
unpad_y8n8 [] = []
unpad_y8n8 (x:xs) = (unpad_row_y8n8 x) : unpad_y8n8 xs

-- Input:
-- String - padded row (length N + 2)

-- Output:
-- String - unpadded row (* removed)

-- unpads a row by removing *
unpad_row_y8n8 :: String -> String 
unpad_row_y8n8 [] = []
unpad_row_y8n8 (x:xs)
	| x == '*'  = unpad_row_y8n8 xs
	| otherwise = x:unpad_row_y8n8 xs


-- 5. Other Functions

-- INPUT: current player

-- OUTPUT: other player

-- swap the current player with the other player
swap_player_y8n8 :: Char -> Char
swap_player_y8n8 'W' = 'B'
swap_player_y8n8 'B' = 'W'
	
-- INPUT: 
-- [[String]] - list of boards
-- [[String]] - list of seen boards

-- OUTPUT: list of boards with no duplicates/traversed board
	
-- removes all the empty, duplicate, and traversed boards
remove_duplicates_y8n8 :: [[String]] -> [[String]] -> [[String]]
remove_duplicates_y8n8 boards path
	|null boards			    = []
	|(null (head boards)) ||
	  (elem (head boards) path)	= remove_duplicates_y8n8 (tail boards) path
	|otherwise				    = ((head boards):(remove_duplicates_y8n8 (tail boards) ((head boards):path)))

	
-- INPUT:
-- [String] - board in a multiple lines format
-- Char - current player
-- Int - dimension of the board
-- [[String]] - list of boards already travelled

-- OUTPUT: list of boards

--returns True if there's no more moves, False otherwise
no_legal_move_y8n8 :: [String] -> Char -> Int -> [[String]] -> Bool
no_legal_move_y8n8 bd c n path =
	null(state_gen_y8n8 bd c n path)

-- INPUT:
-- [String] - list of rows

-- OUTPUT:
-- String - board string

-- given a board in a list of rows, combine into a single string board
rows_to_board_y8n8 :: [String] -> String
rows_to_board_y8n8 [] = []
rows_to_board_y8n8 xs = foldl1 (++) xs

-- INPUT:
-- Int - dimension of board
-- String - board string

-- OUTPUT:
-- [String] - list of rows

-- split up a board into a list of its rows
board_to_rows_y8n8 :: Int -> String -> [String] -- the order here is so we can map it on a list of boards easily
board_to_rows_y8n8 _ [] = []
board_to_rows_y8n8 n xs = board_to_rows_helper_y8n8 xs n n -- pass in: first row's length is n (by defn)

-- INPUT:
-- String - board string
-- Int - dimension of board
-- Int - length of current row

-- OUTPUT:
-- [String] - list of rows
board_to_rows_helper_y8n8 :: String -> Int -> Int -> [String]
board_to_rows_helper_y8n8 [] _ _ = []
board_to_rows_helper_y8n8 xs n l 
	| l == 2 * n - 1 	= --longest row, start making length shorter
		(take l xs) : board_to_rows_helper_y8n8 (drop l xs) n (1 - l) -- (l - 1), then make it negative to keep track
	| l < 0				= -- if l is negative, we're decreasing length
		(take (abs l) xs) : board_to_rows_helper_y8n8 (drop (abs l) xs) n (l + 1) -- keep shortening rows by adding 1 to length (< 0)
	| otherwise			= -- we're increasing length
			(take l xs) : board_to_rows_helper_y8n8 (drop l xs) n (l + 1) -- keep increasing length, add 1 to length (> 0)

-- 6. Test Cases

-- even start on n = 3
test1 = "WWW-WW-------BB-BBB" 

-- obvious jump for B
test2 = "WWWBB----W---B--B--"
test2_r = (board_to_rows_y8n8 3 test2)
test2_next = state_gen_y8n8 test2_r 'B' 3 [test2_r]
test2_next_r = map (board_to_rows_y8n8 3) test2_next 

-- no moves for B
test3 = "-WB--WW-----WWWWBWB"

-- jump win for B
test4 = "WW--W----B--B-B----"

-- rotation test
testrot = 
	["123",
	 "4567",
	 "89012",
	  "3456",
	   "789"]

-- unity test
testunity = 
	["-B-",
	 "-BB-",
	 "-----",
	  "W--W",
	   "-W-"]

-- unity vs take test
testweight =
	["B--",
	 "-BB-",
	 "-B---",
	  "-WW-",
	   "-W-"]

