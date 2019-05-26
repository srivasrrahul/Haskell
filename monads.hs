inc :: [Int] -> [Int]
inc = map (+1)
sq = map (^2)


data MyMaybe a = MyNothing | MyJust a deriving Show
class MyFunctor t where
	myfmap :: (a -> b) -> t a -> t b


instance MyFunctor MyMaybe where
	myfmap f MyNothing = MyNothing
	myfmap f (MyJust x) = MyJust (f x)

instance MyFunctor [] where
	myfmap = map


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show


instance MyFunctor Tree where
	myfmap f (Leaf x) = Leaf (f x)
	myfmap f (Node l r) = Node (myfmap f l) (myfmap f r) 

instance MyFunctor IO where
	myfmap g mx = do 
		x <- mx
		return (g x)


newinc :: MyFunctor t => t Int -> t Int
newinc = myfmap (+1)

data DataTree a = DataLeaf | DataNode (DataTree a) a (DataTree a) deriving Show

instance MyFunctor DataTree where
	myfmap f DataLeaf = DataLeaf
	myfmap f (DataNode l c r) = DataNode (myfmap f l) (f c) (myfmap f r)


instance MyFunctor ((->) a) where
	myfmap f g = f . g



class MyFunctor t => MyApplicative t where
	mypure :: a -> t a
	combine :: t (a -> b) -> t a -> t b












