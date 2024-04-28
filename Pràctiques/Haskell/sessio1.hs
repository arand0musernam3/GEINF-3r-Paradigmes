
-- 1.1
xor :: Bool -> Bool -> Bool
xor a b = if a
    then not b
    else b

--xor a b = a /= b

--xor = (/=)
--xor a = (/= a)


-- 1.2

-- 1.3
maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 a b c = max (max a b) c

-- 1.4
maxim4 :: Integer -> Integer -> Integer -> Integer -> Integer
maxim4 a b c d = max (maxim3 a b c) d

-- 1.7
media3 :: (Fractional p) => p -> p -> p -> p
media3 a b c = (a+b+c)/3

boolToInt:: Bool -> Integer
boolToInt False = 0
boolToInt True = 1

cuantosSobreMedia3:: (Fractional p, Ord p) => p -> p -> p -> Integer
cuantosSobreMedia3 a b c = (boolToInt (a > avg)) + (boolToInt (b > avg)) + (boolToInt (c > avg))
    where avg = (a+b+c)/3

-- 1.8
producto :: Integer -> Integer -> Integer
producto _ 0 = 0
producto 0 _ = 0
producto a b
  | (b < 0 && a < 0) = producto (-a) (-b)
  | (b < 0) = producto b a
  | otherwise = (a) + (producto a (b-1))

-- 1.9
buscarDesde :: Integer -> Integer -> Integer

buscarDesde i n = if (i*i <= n)
    then buscarDesde (i+1) n
    else (i-1)

raiz :: Integer -> Integer
raiz = buscarDesde 1


-- 1.10

pot2 :: Integer -> Integer

pot2 a
  | ((mod a 2) == 0) = (a * a)
  | otherwise = 2*(pot2 (a-1))

-- 1.5
tresDiferentes :: (Eq a) => a -> a -> a -> Bool

tresDiferentes a b c = (a /= b) && (a /= c) && (b /= c)

-- 1.6
cuatroIguales :: (Eq a) => a -> a -> a -> a -> Bool
cuatroIguales a b c d = (a==b) && (b==c) && (c==d)

-- Distancia euclidiana
distance :: (Double, Double) -> (Double, Double) -> Double
distance (a,b) (c,d) = sqrt ((c-a)*(c-a) + (d-b) * (d-b))

--nSum

nSum :: Num a => [a] -> a
nSum [] = 0
nSum (x:y) = (nSum y + x)

-- Interpreter
interpreter :: [String] -> [String]
interpreter y = iInterpreter y 0 0

iInterpreter :: [String] -> Integer -> Integer -> [String]
iInterpreter [] _ _ = []
iInterpreter (a:b) x y
  | (a == "up") = iInterpreter b x (y+1)
  | (a == "down") = iInterpreter b x (y-1)
  | (a == "left") = iInterpreter b (x-1) y
  | (a == "right") = iInterpreter b (x+1) y
  | (a == "printX") = (show x : iInterpreter b x y)
  | (a == "printY") = (show y : iInterpreter b x y)
  | otherwise = iInterpreter b x y