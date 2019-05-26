data Op = Add | Sub | Mul | Div
instance Show Op where
	show Add  = "+"
	show Sub = "-"
	show Mul = "*"
	show Div = "\\"


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
	show (Val n) = show n
	show (App o e1 e2) = brak e1 ++ show o ++ brak e2 where
		brak (Val n) = show n
		brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App o e1 e2) = (values e1) ++ (values e2)

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App op e1 e2) = [apply op l r | l <- eval e1, r <- eval e2, valid op l r]

subs :: [a] -> [[a]]
subs [] = [[]]
--subs (x:[]) = [[x]]
subs (x:xs) = map (x:) recur  ++ recur where
	recur = subs xs


interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:[]) = [x:y:[],y:x:[]]
interleave x (y:ys) = [(x:y:ys)] ++ map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:[]) = [[x]]
perms (x:xs) = concat (map (interleave x) pss) where
 	pss = perms xs


choices :: [a] -> [[a]]

choices = concat . map perms . subs

my_choices :: [a] -> [[a]]
my_choices x = [y | y <- concat (map perms (subs x)) ]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && (eval e) == [n]







