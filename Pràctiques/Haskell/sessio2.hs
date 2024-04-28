-- Ex 1: defineix els operadors --> , <--> (implicacio i equivalencia)
-- --> te prioritat 2
-- <--> te prioritat 1
-- Tots dos son associatius a l'esquerra
-- El seu tipus es Bool -> Bool -> Bool

infixl 2 -->
infixl 1 <-->

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b

(<-->) :: Bool -> Bool -> Bool
a <--> b = (not a || b) && (not b || a)

-- Ex 2: define a version of map that takes three arguments: a function
-- and two lists. It returns a list. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching.
--
-- Create some function that uses `map2` and use a lambda expression
-- as first parameter of `map2`
--
-- Create some function that uses `map2` and use a built in operator
-- as first parameter of `map2`

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] [] = []
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (a:xa) (b:xb) = (f a b) : map2 f xa xb

-- ghci> map2 (+) [1,2,3,4,5] [1,2,3,4]
-- [2,4,6,8]
-- ghci> map2 (+) [1,2,3,4,5] [0,0,0]
-- [1,2,3]
-- ghci> map2 (\x y -> x) [1,2,3,4,5] [0,0,0]
-- [1,2,3]
-- ghci> map2 (\x y -> y) [1,2,3,4,5] [0,0,0]
-- [0,0,0]




-- Ex 3: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (a:xa) = 
    case f a of
        Nothing -> maybeMap f xa
        Just b -> b : maybeMap f xa

