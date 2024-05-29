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
valorDe _ [] = error "No s'ha trobat la variable"
valorDe n ((a :-> b) : xs) = if n == a then b else valorDe n xs

avaluar :: Prop -> Assignacio -> Bool
avaluar (Const b) _ = b
avaluar (Var x) as = valorDe x as
avaluar (No p) as = not (avaluar p as)
avaluar (p :/\ q) as = avaluar p as && avaluar q as
avaluar (p :\/ q) as = avaluar p as || avaluar q as

-- Exercici 2
(+:) :: (Eq a) => a -> [a] -> [a]
(+:) a b
  | a `elem` b = b
  | otherwise = a : b

(+++) :: (Eq a) => [a] -> [a] -> [a]
(+++) a b = foldr (+:) b a

variables :: Prop -> [Nom]
variables (Const _) = []
variables (Var x) = [x]
variables (No p) = variables p
variables (p :/\ q) = variables p +++ variables q
variables (p :\/ q) = variables p +++ variables q

-- Exercici 3
assignacionsPossibles :: [Nom] -> [Assignacio]
assignacionsPossibles n = sequence (map (\x -> [x :-> True, x :-> False]) n)
-- Es pot simplificar a "assignacionsPossibles = mapM (\x -> [x :-> True, x :-> False])"

-- Una alternativa sense map és la següent:
-- assignacionsPossibles [] = [[]]
-- assignacionsPossibles (x:xs) = [(x :-> b) : s | b <- [True,False], s <- assignacionsPossibles xs]

-- Exercici 4 
esSatisfactible :: Prop -> Bool
esSatisfactible p = or (map (avaluar p) (assignacionsPossibles (variables p)))
-- Es pot simplificar a "esSatisfactible p = any (avaluar p) (assignacionsPossibles (variables p))""

-- Exercici 5
esTautologia :: Prop -> Bool
esTautologia p = and (map (avaluar p) (assignacionsPossibles (variables p)))
-- Es pot simplificar a "esTautologia p = all (avaluar p) (assignacionsPossibles (variables p))"

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

instance Eq Assig where
  a :-> b == c :-> d = a == c && b == d

assignacionsIguals :: Assignacio -> Assignacio -> Bool
assignacionsIguals a1 a2 = (a1 +++ a2) == a2 && (a2 +++ a1) == a1

-- Exercici 9
sat :: Prop -> Maybe Assignacio
sat p
  | esSatisfactible p = Just (head [a | a <- assignacionsPossibles (variables p), avaluar p a])
  | otherwise = Nothing

-- Proposem també aquesta solució menys elegant, però més eficient (doncs s'atura a la primera solució)
{-
sat :: Prop -> Maybe Assignacio
sat p = sat' p (assignacionsPossibles (variables p))

sat' :: Prop -> [Assignacio] -> Maybe Assignacio
sat' _ [] = Nothing
sat' p (x : xs)
  | avaluar p x = Just x
  | otherwise = sat' p xs
-}

-- Exercici 10
type Weight = Int

data WProp = Hard Prop | Soft Prop Weight


me1, me2, me3, me4 :: [WProp]
me1 = [Soft (Var "x") 10, Soft (No (Var "x")) 4]
me2 = [Hard (Var "x"), Hard (Var "y"), Hard ((No (Var "x")) :\/ (No (Var "y")))]
me3 =
  [ Hard (Var "x"),
    Hard ((No (Var "x")) :\/ (No (Var "y"))),
    Soft (Var "x") 10,
    Soft (No (Var "x")) 4,
    Soft (Var "y") 5,
    Soft (Var "z") 10,
    Soft (No (Var "z")) 4
  ]
me4 =
  [ Soft ((No (Var "x")) :\/ (No (Var "y"))) 4,
    Soft ((Var "x") :\/ (Var "y")) 4,
    Soft ((No (Var "x")) :\/ (No (Var "z"))) 3,
    Soft ((Var "x") :\/ (Var "z")) 3,
    Soft ((No (Var "z")) :\/ (No (Var "y"))) 5,
    Soft ((Var "z") :\/ (Var "y")) 5
  ]

maxSat :: [WProp] -> Maybe (Assignacio, Weight)
maxSat ls = returnMin [foldr (f x) (Just ([], 0)) ls | x <- lass]
  where
    lass = assignacionsPossibles (allVariables ls)
    f :: Assignacio -> WProp -> Maybe (Assignacio, Weight) -> Maybe (Assignacio, Weight)
    f _ _ Nothing = Nothing
    f assig (Soft p w) (Just(_, z)) = if avaluar p assig
      then Just (assig, z)
      else Just (assig, w+z)
    f assig (Hard p) (Just(_,z)) = if avaluar p assig
      then Just (assig, z)
      else Nothing

returnMin :: [Maybe (Assignacio, Weight)] -> Maybe (Assignacio, Weight)
returnMin = foldr f Nothing
  where
    f :: Maybe(Assignacio, Weight) -> Maybe(Assignacio, Weight) -> Maybe (Assignacio, Weight)
    f Nothing d = d
    f (Just x) Nothing = Just x
    f (Just (a, w)) (Just (b,z)) = if w < z then Just (a,w) else Just (b,z)

allVariables :: [WProp] -> [Nom]
allVariables [] = []
allVariables (x : xs) =
  case x of
    Hard x -> variables x +++ allVariables xs
    Soft x _ -> variables x +++ allVariables xs

instance Show WProp where
  show (Hard p) = show p
  show (Soft p w) = "(" ++ show p ++ "," ++ show w ++ ")"