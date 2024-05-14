import Data.List
import Control.Monad (replicateM)


type Nom = String

infixl 7 :/\

infixl 6 :\/

data Prop
  = Const Bool
  | Var Nom
  | No Prop
  | Prop :/\ Prop
  | Prop :\/ Prop

e1, e2, e3 :: Prop
e1 = Const True :/\ Var "a"
e2 = Var "a" :/\ No (Var "a")
e3 = (Var "a" :/\ Var "b") :\/ No (Var "c")

-- Exercici 1
data Assig = Nom :-> Bool

type Assignacio = [Assig]

valorDe :: Nom -> Assignacio -> Bool
valorDe _ [] = False
valorDe n ((a :-> b) : xs) = if n == a then b else valorDe n xs

avaluar :: Prop -> Assignacio -> Bool
avaluar (Const b) _ = b
avaluar (Var x) as = valorDe x as
avaluar (No p) as = not (avaluar p as)
avaluar (p :/\ q) as = avaluar p as && avaluar q as
avaluar (p :\/ q) as = avaluar p as || avaluar q as

-- Exercici 2
(+:) :: Eq a => a -> [a] -> [a]
infix 5 +:
(+:) a b
  | a `elem` b = b
  | otherwise = a:b

(+++) :: Eq a => [a] -> [a] -> [a]
(+++) a b = foldr (+:) b a

variables :: Prop -> [Nom]
variables (Const _) = []
variables (Var x) = [x]
variables (No p) = variables p
variables (p :/\ q) = (variables p) +++ (variables q)
variables (p :\/ q) = (variables p) +++ (variables q)

-- Exercici 3
assignacionsPossibles :: [Nom] -> [Assignacio]
assignacionsPossibles xs = map (f xs) boolCombs
    where
      boolCombs = replicateM (length xs) [True, False]
      f :: [Nom] -> [Bool] -> Assignacio
      f [] [] = []
      f (n:ns) (b:bs) = n :-> b : f ns bs
-- https://chat.openai.com/share/8b34a5c4-2259-4bab-88a1-fc8bcfd1ea91


-- Exercici 4 PASSAR A SOLUCIÓ AMB MAP I OR
esSatisfactible :: Prop -> Bool
esSatisfactible p = esSatisfactible' p (assignacionsPossibles (variables p))

esSatisfactible' :: Prop -> [Assignacio] -> Bool
esSatisfactible' _ [] = False
esSatisfactible' p (x:xs)
  | avaluar p x = True
  | otherwise = esSatisfactible' p xs


-- Exercici 5 PASSAR A SOLUCIÓ AMB MAP I AND
esTautologia :: Prop -> Bool
esTautologia p = esTautologia' p (assignacionsPossibles (variables p))

esTautologia' :: Prop -> [Assignacio] -> Bool
esTautologia' _ [] = True
esTautologia' p (x:xs)
  | not (avaluar p x) = False
  | otherwise = esTautologia' p xs

-- Exercici 6
infixl 5 --> 
(-->) :: Prop -> Prop -> Prop
a --> b = No a :\/ b

infixl 5 ==>
(==>) :: Prop -> Prop -> Bool
a ==> b = esTautologia (a --> b)

-- Exercici 7
infixl 4 <-->
(<-->) :: Prop -> Prop -> Prop
a <--> b = (a --> b) :/\ (b --> a)

infixl 4 <==>
(<==>) :: Prop -> Prop -> Bool
a <==> b = esTautologia (a <--> b)

-- ghci> No (No (Var "a")) <==> Var "a"
-- True
-- ghci> No (Var "a" :/\ Var "b") <==> No (Var "a") :\/ No (Var "b")
-- True
-- ghci> No (Var "a" :\/ Var "b") <==> No (Var "a") :/\ No (Var "b")
-- True
-- ghci> Var "a" --> Var "b" <==> No (Var "b") --> No (Var "a")
-- True
-- ghci> Var "a" :/\ (Var "a" --> Var "b") <==> Var "b"
-- False
-- ghci> Var "a" :/\ (Var "a" --> Var "b") ==> Var "b"
-- True

-- Exercici 8
instance Show Prop where
    show (Const b) = show b
    show (Var x) = x
    show (No p) = "!" ++ show p
    show (p :/\ q) = "(" ++ show p ++ " /\\ " ++ show q ++ ")" 
    show (p :\/ q) = "(" ++ show p ++ " \\/ " ++ show q ++ ")" 

instance Show Assig where
    show (a :-> b) = a ++ ":=" ++ show b