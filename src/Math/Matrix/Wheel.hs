module Wheel where

import Data.Bool (bool)

data Wheel a = Axis | Section { idx :: Int, content :: a, next :: Wheel a }

unnext :: Wheel a -> Maybe (a, Wheel a)
unnext wheel = case wheel of
    Axis              -> Nothing
    (Section _ x nxt) -> Just (x, nxt)

change :: a -> Wheel a -> Wheel a
change _ Axis = Axis
change x (Section i _ nxt) = ref
    where
    ref = Section i x $ mount i nxt

    mount i sec = if idx sec == i then ref
        else Section (idx sec) (content sec) $ mount i (next sec)

instance Functor Wheel where
    fmap f Axis = Axis
    fmap f (Section i x nxt) = ref
        where
        ref = Section i (f x) $ mount i nxt

        mount _ Axis = Axis
        mount i sec = if idx sec == i then ref 
            else Section (idx sec) (f $ content sec) $ mount i (next sec)

instance (Show a) => Show (Wheel a) where
    show Axis              = "Axis"
    show (Section i x nxt) = ref
        where
        ref = "ref = Section {idx="++(show i)++",content="++(show x)++"} -> "++(mount i nxt)

        mount _ Axis = "Axis"
        mount i sect = if idx sect == i then "ref" else 
            "Section {idx="++(show $ idx sect)++",content="++(show $ content sect)++"} -> "
            ++ (mount i (next sect))

instance Semigroup a => Monoid (Wheel a) where
    mempty :: Wheel a 
    mempty = Axis
    
instance Semigroup (Wheel a) where
    (<>) :: Wheel a -> Wheel a -> Wheel a
    (<>) Axis b    = b
    (<>) a    Axis = a
    (<>) sec1 sec2 = fromList (toList sec1 ++ toList sec2)

isAxis :: Wheel a -> Bool
isAxis Axis = True
isAxis _    = False

remove :: Int -> Wheel a -> Wheel a
remove _ Axis = Axis
remove 0 sect = sect
remove i sect = remove (i-1) $ (fromList . tail . toList) sect

fromList :: [a] -> Wheel a
fromList []     = Axis
fromList (x:xs) = ref 
    where 
    ref = Section 0 x $ mount 1 xs ref

    mount :: Int -> [a] -> Wheel a -> Wheel a
    mount _ []     lst = lst
    mount n (y:ys) lst = Section n y $ mount (n+1) ys lst

toList :: Wheel a -> [a]
toList Axis = []
toList sec  = ref
    where 
    ref = content sec : mount (idx sec) (next sec)
        --if ((==) 0 . idx . next) sec then [] else (toList . next) sec 
    mount i lst = if ((==) i . idx ) lst then [] else content lst : (mount i . next) lst 

section :: (a -> Bool) -> Wheel a -> Maybe a
section _ Axis = Nothing
section f sect = search (idx sect) f sect 

    -- hide it (please)
search :: Int -> (a -> Bool) -> Wheel a -> Maybe a
search i f Axis = Nothing
search i f sect = case (f . content) sect of
    True  -> Just $ content sect
    False -> if ((==) i . idx . next) sect then Nothing else search i f (next sect)

search' :: Int -> (Wheel a -> Bool) -> Wheel a -> Maybe a
search' i f Axis = Nothing
search' i f sect = case f sect of
    True  -> Just $ content sect
    False -> if ((==) i . idx . next) sect then Nothing else search' i f (next sect)
--referenece = maybe False ((/=) 0) . fmap fst . uncons $ [1,2,3]


