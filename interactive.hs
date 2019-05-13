
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
           