--Ex 1 github amb llistes de comprensió

joinToLength :: Int -> [String] -> [String]
joinToLength x s = [a++b | a <- s, b <- s, (length a + length b) == x]

-- Explicació foldr

-- foldr (+) 0 [1,2,3,4] és el mateix que:
-- 1 + ( 2 + ( 3 + (4 + 0 ) ) ) 

-- foldl (+) 0 [1..4]
-- ((((0+1) + 2) + 3) + 4)

-- Implementació del concat amb un fold
-- ghci> concat [[1,2],[3,4]]
-- [1,2,3,4]

-- ghci> foldr (++) [] [[1,2],[3,4]]
-- [1,2,3,4]

-- Per tant, foldr seguit de operació, element neutre i llista on operar

-- Implementació del reverse
-- RECORDAR LA X ÉS TIPUS ELEMENT DE LA SEGONA LLISTA MENTRE QUE LA Y ÉS DEL TIPUS ELEMENT NEUTRE

-- ghci> foldr (\x y -> y++[x]) [] [1,2,3,4]
-- [4,3,2,1]

-- MOLT MÉS EFICIENT PERQUÈ ÉS O(1) I NO O(n)
-- ghci> foldl (\x y -> y:x) [] [1,2,3,4]
-- [4,3,2,1]


-- Ex 2: A laboratory has been collecting measurements. Some of the
-- measurements have failed, so the lab is using the type
--   Either String Int
-- to track the measurements. A Left value represents a failed measurement,
-- while a Right value represents a succesful one.
--
-- Compute the sum of all succesful measurements. If there are
-- succesful measurements, return the sum wrapped in a Right, but if
-- there are none, return Left "no data".
--
-- Examples:
--   sumSuccess [Right 1, Left "it was a snake!", Right 3]
--     ==> Right 4
--   sumSuccess [Left "lab blew up", Left "I was sick"]
--     ==> Left "no data"
--   sumSuccess []
--     ==> Left "no data"
--
--  Hint: this is a great use for folds
--
-- Give the signature of the function

sumSuccess :: [Either String Integer] -> Either String Integer
sumSuccess = foldl (f) (Left "no data")
    where 
        f (Left _) d = d
        f (Right x) (Left _) = Right x
        f (Right x) (Right y) = Right(x+y)

-- A la funció f del foldl, l'acumulat és el primer paràmetre i l'actual és el segon
-- En el cas del foldr, l'actual és el primer paràmetre i l'acumulat és el segon

-- Ex 3: recall the binary function composition operation
-- (f . g) x = f (g x). In this exercise, your task is to define a function
-- that takes any number of functions given as a list and composes them in the
-- same order than they appear in the list.
--
-- Examples:
--   multiCompose [] "foo" ==> "foo"
--   multiCompose [] 1     ==> 1
--   multiCompose [(++"bar")] "foo" ==> "foobar"
--   multiCompose [reverse, tail, (++"bar")] "foo" ==> "raboo"
--   multiCompose [(3*), (2^), (+1)] 0 ==> 6
--   multiCompose [(+1), (2^), (3*)] 0 ==> 2
--
--  Hint: this is a great use for folds
--  Hint2: id retorna exactament el que li passes, si és una funció també!
--

multiCompose :: [t -> t] -> (t -> t)
multiCompose = foldr (.) id
-- estic plorant, és precios


-- Idees útils per la pràctica!!