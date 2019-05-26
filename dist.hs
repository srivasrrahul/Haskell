import qualified Data.Map as Map
type LatLong = (Double,Double)

locationsDB :: Map.Map String LatLong
locationsDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]


toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
 where rlat = toRadians lat
       rlong = toRadians long


haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where (rlat1,rlong1) = latLongToRads coords1
       (rlat2,rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1
       a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
       c = 2 * atan2 (sqrt a) (sqrt (1-a))
       earthRadius = 3961.0


printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Notthing"
printDistance (Just distance) = putStrLn (show distance ++ " miles")


addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a+b)
addMaybe _ _ = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe = fmap (1+) 

--data Function a = ((->) a)
-- instance Applicative ((->) t) where
-- 	--pure :: x -> Function x
-- 	pure = (\\)
-- 	--(<*>) :: (\t)
-- 	x <*> y = x . y
-- instance MyApplicative ((->) a) => Applicative ((->) a)
-- 	--pure :: x -> ((->) x)
-- 	pure x = ((->) x)  
-- 	(\x -> x a) <*> (\y -> y b) =  


data Tree a = Node a [Tree a] deriving Show

instance Functor Tree where
	fmap f (Node x z) = Node (f x) [(fmap f y) | y <- z]