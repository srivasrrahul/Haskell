reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just (reverse s)

data Box a = Box a deriving Show
instance Functor Box where
	fmap f (Box x)= Box (f x) 


morePresents :: Int -> Box a -> Box [a]
morePresents n  = fmap (replicate n) 

myBox :: Box Int
myBox = Box 1

wrapped :: Box Int -> Box (Box Int)
wrapped  = fmap (\x -> Box x)  

unwrapped :: Box (Box Int) -> Box Int
unwrapped = fmap (\(Box x) -> x)