import Data.Char

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)


myproduct :: [Integer] -> Integer
myproduct [] = 1
myproduct (x:xs) = x * myproduct(xs)

add_curry :: Integer -> (Integer -> Integer)
add_curry x y = x + y

add_1 :: Integer -> Integer
add_1 = add_curry 1

halve :: [a] -> ([a],[a])

halve lst = splitAt ((length lst ) `div` 2) lst

my_last :: [a] -> a
my_last (x:[]) = x
my_last (x:xs) = my_last xs


my_init (x:[]) = []
my_init (x:xs) = x:(my_init xs)

my_third :: [a] -> a
my_third xs = head (tail (tail xs))

luhndouble :: Int -> Int
luhndouble x = 
	let d = 2*x in 
    if d > 9 then d-9
    	else d

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = 
	let result = (luhndouble a) + (luhndouble b) + (luhndouble c) in
	let last_digit = result `mod` 10 in
	if last_digit == d then True
		else False

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m],y <- [0..n]]

square  :: Int -> [(Int,Int)]
square m = [(x,y) | (x,y) <- (grid m m), x /= y ]

--replicate using list comprehension
gen_set :: Int -> a -> [a]
gen_set 0 x = []
gen_set n x = [x] ++ (gen_set (n-1) x)
my_replicate :: Int -> a -> [a]
my_replicate n x = [y | y <- (gen_set n x)]
	

pythagorean :: Int -> [(Int,Int,Int)]
pythagorean n = [(x,y,z) | x <- [1..n],y <- [1..n], z <- [1..n],x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n],n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n], sum(factors(x))-x == x]

prime :: Int -> Bool
prime n = factors(n) == [1,n]

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct lst1 lst2 = sum ([ (x*y) | (x,y)<-(zip lst1 lst2)])

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : (insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater
               where
               	smaller  = [a | a<- xs, a<=x]
               	greater = [a | a<-xs,a > x]

sumdown :: Int -> Int

sumdown 0 = 0
sumdown n = n + (sumdown (n-1))


my_exp :: Int -> Int -> Int
my_exp a 0 = 1
my_exp a b = a * (my_exp a (b-1))

euclid :: Int -> Int -> Int
euclid x 0 = x
euclid x 1 = 1
euclid a b | a > b = euclid b (a `mod` b)   
           | a < b = euclid b a
           | otherwise = a


my_and :: [Bool] -> Bool
my_and [] = True
my_and (x:xs) | x == True = my_and xs
              | otherwise = False


my_concat :: [[a]] -> [a]
my_concat (x:[]) = x
my_concat (x : xs) = x ++ (my_concat xs) 

my_replicate_recr :: Int -> a -> [a]
my_replicate_recr 0 x = [x]
my_replicate_recr n x = [x] ++ (my_replicate_recr (n-1) x)

nth_element :: Int -> [a] -> a
nth_element n (x:xs) | n == 1  = x
                     | otherwise = nth_element (n-1) xs

my_elem :: Eq a => a -> [a] -> Bool
my_elem x [] = False
my_elem x (y:ys) | x == y = True
                 | otherwise = my_elem x ys


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys) | x <= y = x : (merge xs (y:ys))
                    | x > y = y : (merge (x:xs) ys)

half :: [a] -> ([a],[a])
half lst = splitAt ((length lst)`div` 2) lst
msort :: Ord a => [a] -> [a]
msort (x:[]) = [x]
msort lst = merge (msort left) (msort right)  
               where
               	(left,right) = (half lst)

main = do 
	--let x = my_third [1,2,3,4]
	--let x = luhndouble 6
	let x = luhn 4 7 8 3
	print x

