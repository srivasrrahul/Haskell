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

my_sum :: [Int] -> Int
my_sum (x:[]) = x
my_sum (x:xs) = x + (my_sum xs)


my_take :: Int -> [a] -> [a]
my_take n [] = []
my_take n (x:xs) | n == 0 = []
                 | otherwise = x : (my_take (n-1) xs)

select_last :: [a] -> a
select_last (x:[]) = x
select_last (x:xs) = select_last xs

my_product :: Num a => [a] -> a
my_product = foldr (*) 1


my_sum_fold :: Num a => [a] -> a
my_sum_fold = foldr (+) 0

my_length :: [a] -> Int
my_length = foldr (\_ n -> 1 + n) 0

my_reverse :: [a] -> [a]
my_reverse = foldr (\x val -> val ++ [x]) []

my_sum_foldl :: Num a => [a] -> a
my_sum_foldl = foldl (+) 0

my_product_foldl :: Num a => [a] -> a
my_product_foldl = foldl (*) 1

my_or :: [Bool] -> Bool
my_or = foldl (||) True

my_and_foldl :: [Bool] -> Bool
my_and_foldl = foldl (&&) True

my_length_foldl :: [a] -> Int
my_length_foldl = foldr (\x v -> v +1 ) 0

my_reverse_foldl :: [a] -> [a]
my_reverse_foldl = foldl (\x v -> [v] ++ x) []

list_comprehension_higher_order :: (a -> a) -> (a -> Bool) -> [a] -> [a]
list_comprehension_higher_order f p  = (filter p) . (map f)

my_all :: (a -> Bool) -> [a] -> Bool
my_all p x = length (filter p x) == length x

my_any :: (a -> Bool) -> [a] -> Bool
my_any p x = length (filter p x) /= 0

my_take_while :: (a -> Bool) -> [a] -> [a]
my_take_while p m = t 
	    where (t,_) = 
		   foldl (\(y,z) x -> 
		     if z == True then (y,z)
			 else
				if (p x) == True then
					(y ++ [x],False) else
						(y,True)
		    ) ([],False) m


my_drop_while :: (a -> Bool) -> [a] -> [a]
my_drop_while p m = t 
	    where (t,_) = 
		   foldl (\(y,z) x -> 
		     if z == True then (y ++ [x],z)
			 else
				if (p x) == True then
					(y,False) else
						(y ++ [x],True)
		    ) ([],False) m
	                            
my_map :: (a ->a) -> [a] -> [a]
my_map f  = foldr (\x y -> [f x] ++ y) []

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter p = foldr (\x y -> if (p x) then [x] ++ y else y) []


dec2int :: [Int] -> Int
dec2int = foldl (\v x -> 10*v + x) 0

-- my_curry :: (x -> y) -> (y -> z) -> (x -> z)
-- my_curry f1 f2 = f1 . f2

-- altmap :: (a->b) -> (a -> b) -> [a] -> (Bool,[b])
-- -- altmap f1 f2 = foldr (\x (y,z) -> 
-- -- 		if y == True then (False, [(f1 x)] ++ z)
-- -- 			else (True, [(f2 x)] ++ z)) (True,[])
altmap :: (a->b) -> (a -> b) -> [a] -> [b]
altmap f1 f2  z = t where
	(_,t) = foldr (\x (y,z) -> 
		if y == True then (False, [(f1 x)] ++ z)
			else (True, [(f2 x)] ++ z)) (True,[]) z


---ADT
data Nat = Zero | Succ Nat deriving Show
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

int2nat :: Int -> Nat 
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add_nat :: Nat -> Nat -> Nat
add_nat m n = int2nat ((nat2int m) + (nat2int n))

add_nat_add_rec :: Nat -> Nat -> Nat
add_nat_add_rec Zero n = n
add_nat_add_rec (Succ m) n = Succ (add_nat_add_rec m n)

add_nat_till :: Nat -> Nat -> Nat -> Nat
add_nat_till m (Succ Zero) acc = acc
add_nat_till m (Succ n) acc = add_nat_till m n (add_nat_add_rec acc m)

add_nat_mul_rec :: Nat -> Nat -> Nat
add_nat_mul_rec Zero _ = Zero
add_nat_mul_rec _ Zero = Zero
add_nat_mul_rec m (Succ Zero) = m
add_nat_mul_rec m n =  add_nat_till m n m


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
occurs :: Eq a => a -> Tree a -> Bool
occurs val (Leaf x) = val == x
occurs val (Node left b right) = (occurs val left) || b == val || (occurs val right)

occurs_search :: Ord a => a -> Tree a -> Bool
occurs_search val (Leaf x) = val == x
occurs_search val (Node left b right) | b == val = True
                                      | val < b = occurs_search val left
                                      | val > b = occurs_search val right

leaves :: Tree a -> Int 
leaves (Leaf x) = 1
leaves (Node left x right) = 1 + (leaves left) + leaves (right)

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node left x right) = left_balanced && right_balanced && abs((leaves left) - (leaves right)) < 1
              where left_balanced = balanced left
                    right_balanced = balanced right

-- split_list :: [a] -> ([a],a,[a])
-- split_list x = (fst split_value,head (snd split_value),tail (snd split_value))
--               where split_value = splitAt ((length x) `div` 2) x



data MyTree a = MyLeaf a | MyNode (MyTree a) (MyTree a) deriving Show

balance :: [a] -> MyTree a
balance (x:[]) = MyLeaf x
balance (x:y:[]) = MyNode (MyLeaf x) (MyLeaf y)
balance x = MyNode (balance left) (balance right)
       where (left,right) = splitAt ((length x) `div` 2) x
		

data Expr = Val Int | Add Expr Expr deriving Show

--folde :: (Int -> a) -> (a->a->a) -> Expr-> a


main = do 
	--let x = my_third [1,2,3,4]
	--let x = luhndouble 6
	let x = luhn 4 7 8 3
	print x

