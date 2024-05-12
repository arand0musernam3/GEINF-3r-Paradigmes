import Data.List


type Nom = String

infixl 7 :/\

infixl 6 :\/

data Prop
  = Const Bool
  | Var Nom
  | No Prop
  | Prop :/\ Prop
  | Prop :\/ Prop
  deriving (Show)

e1, e2, e3 :: Prop
e1 = Const True :/\ Var "a"
e2 = Var "a" :/\ No (Var "a")
e3 = (Var "a" :/\ Var "b") :\/ No (Var "c")

-- Exercici 1
data Assig = Nom :-> Bool deriving (Show)

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

-- Exercici 3
variables :: Prop -> [Nom]
variables (Const _) = []
variables (Var x) = [x]
variables (No p) = variables p