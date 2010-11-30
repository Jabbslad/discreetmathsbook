y = x + 2
x = 4 * 5

square :: Integer -> Integer
square x = x*x

f :: Integer -> String
f 1 = "one"
f 2 = "two"
f 3 = "three"
f _ = "unknown"

nor :: Bool -> Bool -> Bool
nor False False = True
nor _ _ = False 

-- Exercise 3
is_a :: Char -> Bool
is_a 'a' = True
is_a _ = False

-- Exercise 4
is_hello :: String -> Bool
is_hello "hello" = True
is_hello _ = False

-- Exercise 5
trim_leading :: String -> String
trim_leading (' ':xs) = xs
trim_leading xs = xs

twice :: (a->a) -> a -> a
twice f x = f (f x)

{- However, the partial application prod 4 supplies just one argument, 
   and the result of this is a new function that takes a number and multiplies 
   it by 4. -}
prod :: Integer -> Integer -> Integer 
prod x y = x*y
g = prod 4 
p = g 6 
q = twice g 3
