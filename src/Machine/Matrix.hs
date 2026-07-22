module Machine.Matrix where

import qualified System.IO.Error as Error (tryIOError)
import qualified Data.Bool as Bool
import qualified Data.List as List
import Numeric.Natural

coefficient :: (RealFrac a) => a -> a -> a
coefficient = (/) . negate

(!?) :: Natural -> [a] -> Maybe a
(!?) = (.) (fmap fst .  List.uncons) . (drop . fromIntegral)

eliminator :: (RealFrac a) => Line a -> Line a -> Line a
eliminator la lb = maybe lb id $ 
    (idx la) !? (list la)
        >>= \a -> (idx la) !? (list lb)
        >>= \b -> (pure . (*)) (coefficient a b)
        >>= \f -> if b == 0
            then pure lb
            else (pure . fmap roundDecimal2 . flip (+) la . fmap f) lb

elimination :: RealFrac a => Matrix a -> Matrix a
elimination = Matrix . (angle . content) . (fromList . stair (>=) . toList)
    where
    angle :: (RealFrac a) => [Line a] -> [Line a]
    angle []     = []
    angle (x:xs) = x : (angle . adapter x . stair (>=) . map (list . eliminator x)) xs

    adapter :: Line a -> [[a]] -> [Line a]
    adapter = toLine . (succ . idx)

stair :: (Ord a,Num a) => (Natural -> Natural -> Bool) -> [[a]] -> [[a]]
stair _ xs | (length xs) < 2 = xs
stair f xs = (\(a,b) -> order f (stair f a) (stair f b)) . splitAt (length xs `div` 2) $ xs
    where
    order :: (Ord a,Num a) => (Natural -> Natural -> Bool) -> [[a]] -> [[a]] -> [[a]]
    order _ [] bs = bs
    order _ as [] = as
    order g (a:as) (b:bs) 
        | g (counter a) (counter b) = a : order g as (b:bs)
        | otherwise                 = b : order g (a:as) bs
    
counter :: (Num a,Eq a) => [a] -> Natural
counter ls = foldl (\acc (k,x) -> acc + x*2^(k::Integer)) 0 power
   where 
   power    = (reverse . zip [0..] . reverse) binary
   binary   = map (Bool.bool 1 0 . ((==) 0)) ls

toLine :: Natural -> [[a]] -> [Line a]
toLine i = zipWith Line [i..] 

isMatrix :: [[a]] -> Bool
isMatrix []     = True
isMatrix (x:xs) = all ((==) (length x) . length) xs

data Line a = Line 
    { idx  :: Natural
    , list :: [a] } deriving Show

instance Functor Line where
    fmap f line = line { list = map f (list line) }

instance Foldable Line where
    foldr f acc = foldr f acc . list
    length = length . list

instance Num a => Num (Line a) where
    (+) la = Line (idx la) . (\(a,b) -> zipWith (+) a b) . matchSize la
    (-) la = Line (idx la) . (\(a,b) -> zipWith (-) a b) . matchSize la
    (*) la = Line (idx la) . (\(a,b) -> zipWith (*) a b) . matchSize la
    abs = fmap abs
    signum = fmap signum 
    fromInteger x = Line 1 [fromInteger x]

instance Eq a => Eq (Line a) where
    (==) l1 l2 = (==) (idx l1) (idx l2)

matchSize :: Num a => Line a -> Line a -> ([a], [a])
matchSize la lb = (listA,listB)
    where
    lenMax = max (length la) (length lb)
    listA = (take lenMax . (++) (list la) . cycle) [0]
    listB = (take lenMax . (++) (list lb) . cycle) [0]

newtype Matrix a = Matrix { content :: [Line a] }

instance (Show a) => Show (Matrix a) where
    show m = concat $ map (\x -> show x ++ "\n") $ content m

toList :: Matrix a -> [[a]]
toList = map list . content

fromList :: (Eq a, Num a) => [[a]] -> Matrix a
fromList = Matrix . zipWith (\a b -> Line a b) [0..] . fill . List.nub

rowLength :: Matrix a -> Int
rowLength = length . higherRow . toList

colLength :: Matrix a -> Int
colLength = length . content

fill :: Num a => [[a]] -> [[a]]
fill lst = map (\l -> take len $ l ++ (cycle [0])) lst
    where
    len = length . higherRow $ lst

higherRow :: [[a]] -> [a]
higherRow = foldl (\acc l -> Bool.bool l acc (length acc > length l)) [] 

calculate :: Natural -> Double -> Natural -> Matrix Double -> Matrix Double
calculate x k y m = maybe m id $ getMatrixLine x m 
    >>= \l1 -> getMatrixLine y m 
    >>= \l2 -> (pure . insertLine (fmap roundDecimal2 $ (fmap (*k) l2) + l1)) m

getMatrixLine :: Natural -> Matrix a -> Maybe (Line a)
getMatrixLine i = List.find ((==) i . idx) . content

insertLine :: Line a -> Matrix a -> Matrix a
insertLine l = Matrix . aux . content
    where
    aux lst = case List.uncons lst of
        Nothing     -> []
        Just (x,xs) -> if idx x == idx l then l : xs else x : aux xs

removeLine :: Eq a => Natural -> Matrix a -> Matrix a
removeLine i = Matrix . List.delete (Line i []) . content

equalLines :: Eq a => Line a -> Matrix a -> [Line a]
equalLines l = foldl (\acc x -> Bool.bool acc (x:acc) (list l == list x)) [] . content

type Index = Natural

roundDecimal2 :: (RealFrac a) => a -> a
roundDecimal2 x = fromIntegral (round (x * 1e3)::Integer) / 1e3

toText :: (Show a) => Matrix a -> String
toText = unlines . map (unwords . map show . list) . content

fromText :: String -> Matrix Double
fromText = fromList . map (map (read :: String -> Double)) . map words . lines 

load :: FilePath -> IO ()
load = (=<<) manage . Error.tryIOError . readFile
    where
    manage = either print putStrLn

calcInFile :: Double -> Natural -> Natural -> FilePath -> IO ()
calcInFile k x y fp = do 
    result <- Error.tryIOError (readFile fp) 
    either print putStrLn result
    case result of 
        Left  _   -> print result
        Right str -> (save fp . calculate x k y . fromText) str
    readFile fp >>= putStrLn 

save :: (Show a) => FilePath -> Matrix a -> IO ()
save fp = (=<<) manage . Error.tryIOError . writeFile fp . toText
    where
    manage = either print (const (putStrLn "Done!"))

reset :: (Eq a,Num a,Show a) => [[a]] -> IO ()
reset m = (=<<) manage $ Error.tryIOError (writeFile "matrix.data" . toText $ fromList m)
    where
    manage = either print (const (putStrLn "Done!"))

