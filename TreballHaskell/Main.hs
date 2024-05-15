import Data.List
import Control.Monad (replicateM)
import qualified GHC.TypeLits as possibles


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

  
--Exercici 9

sat :: Prop -> Maybe Assignacio
sat p = sat' p (assignacionsPossibles (variables p))

sat' :: Prop -> [Assignacio] -> Maybe Assignacio
sat' _ [] = Nothing
sat' p (x:xs)
  | avaluar p x = Just x
  | otherwise = sat' p xs



-- Exercici 10
type Weight = Int
data WProp = Hard Prop | Soft Prop Weight

me1, me2, me3, me4 :: [WProp]
me1=[Soft (Var "x") 10, Soft (No (Var "x")) 4]
me2=[Hard (Var "x"), Hard (Var "y"), Hard ((No (Var "x")) :\/ (No (Var "y")))]
me3=[Hard (Var "x"), Hard ((No (Var "x")) :\/ (No (Var "y"))),Soft (Var "x") 10, 
 Soft (No (Var "x")) 4, Soft (Var "y") 5, Soft (Var "z") 10, Soft (No (Var "z")) 4]
me4=[Soft ((No (Var "x")) :\/ (No (Var "y"))) 4, Soft ((Var "x") :\/ (Var "y")) 4, 
 Soft ((No (Var "x")) :\/ (No (Var "z"))) 3, Soft ((Var "x") :\/ (Var "z")) 3, 
 Soft ((No (Var "z")) :\/ (No (Var "y"))) 5, Soft ((Var "z") :\/ (Var "y")) 5]


maxSat::[WProp]->Maybe (Assignacio, Weight)
maxSat [] = Nothing
maxSat (Hard x : xs) = 
    case eval of
        Nothing -> Nothing
        Just x -> maxSat xs
    where eval = sat x

-- hauria de ser quelcom de l'estil
-- 1. fer crida que generi, per totes les variables de totes les preposicions, totes les assignacions possibles
--      també afegir un paràmetre amb valor inicial 0 que anirà acumulant totes les soft que no es poden validar
--      no sé si s'haurien de comprovar primer totes les hard i després anar a per les soft amb les assignacions que han quedat lliures
-- 2. per cada crida recursiva, retornar totes les assignacions que han permès validar aquella hard