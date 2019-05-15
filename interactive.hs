import Data.Char
get_my_line :: IO String
get_my_line = do 
	x <- getChar
	if x == '\n' then
		return []
	else
		do
			xs <- get_my_line
			return (x:xs)

put_lst :: String -> IO ()
put_lst [] = return ()
put_lst (x : xs) = do
	putChar x
	put_lst xs

put_my_line :: String -> IO ()
put_my_line [] = return ()
put_my_line (x:xs) = do
	put_lst (x:xs)
	putChar '\n'

my_strlen :: String -> IO ()
my_strlen x = do
	put_my_line (show (length x))


prompt :: String -> IO String
prompt x = do
	putStrLn x
	n <- getLine
	return n


prompt_till_n :: String -> Int -> Int -> IO ()
prompt_till_n x 1 sum_collected = do
	putStrLn x
	l <- getLine
	let d = (read l :: Int)
	putStrLn (show (d+sum_collected))


prompt_till_n x n sum_collected = do
	putStrLn x
	l <- getLine
	let d = (read l :: Int)
	prompt_till_n x (n-1) (sum_collected + d)
	

adder :: IO ()
adder = do
	n <- prompt "How many numbers?"
	let d = (read n :: Int)
	prompt_till_n "Enter number :" d 0
 
	
