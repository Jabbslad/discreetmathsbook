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

-- Exercise 6
toBool :: Int -> Bool
toBool 1 = True
toBool 0 = False

convert :: [Int] -> [Bool]
convert xs = map toBool xs

-- Exercise 7
member0 :: String -> Bool
member0 xs = or (map (=='0') xs)

-- Exercise 8
-- See expanding.txt

-- Exercise 9
addJust :: Maybe Int -> Maybe Int -> Maybe Int
addJust (Just a) (Just b) = Just (a + b)
addJust (Just a) Nothing = Just a
addJust Nothing (Just a) = Just a
addJust Nothing Nothing = Nothing

addMaybe :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addMaybe lst1 lst2 = zipWith addJust lst1 lst2

-- Exercise 10
data Metal = Aluminium | Chromium | Cobalt | Copper | Gold | Iron
  deriving (Eq, Show)

data Coins = OneP Int | TwoP Int | FiveP Int 
  | TenP Int | TwentyP Int
  | FiftyP Int | HundredP Int 
  deriving (Eq, Show)
  
data Universal = BOOL Bool | CHAR Char | INT Int
  deriving (Eq, Show)
  
data Tuples a b c d = Tuple0 | Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d
  deriving (Eq, Show)
  
showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = []
showMaybe (Just x) = show x

bitAnd :: Int -> Int -> Int 
bitAnd 1 1 = 1 
bitAnd x y = 0

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd lst1 lst2 = zipWith bitAnd lst1 lst2

greaterThanN :: [Int] -> Int -> [Int]
greaterThanN xs n = [ x | x <- xs, x > n]

indexOf :: [Int] -> Int -> [Int]
indexOf lst n = [x | x <- [0..(length lst - 1)], lst!!x == n]

compChar :: (Eq a, Num b) => a -> a -> b -> b 
compChar letter x acc = if letter == x then acc + 1 else acc

countLetter :: Char -> [Char] -> Int
countLetter c string = foldr (compChar c) 0 string

-- Exercise 25

-- Exercise 26

revlist :: [a] -> a -> [a]
revlist xs x = x:xs

rev :: [a] -> [a]
rev lst = foldl revlist [] lst