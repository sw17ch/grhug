


module Main where

import Prelude hiding (Maybe(..))
--main = do 
--    print "hello world"




add :: (Num a) => a -> a -> a
add a b = a + b

inc :: (Num a) => a -> a
inc x = add 1 x

f x y = x * x + y

s :: (Num a) => [a]
s = [1, 3, 5, 63, 2]

--s = 1:3:5:63:2:[]
fibs :: [Integer]
fibs = 1:1:zipWith (+) fibs (tail fibs)

--(+):: a -> a -> a
--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--sum [] = 0
--sum (x:xs) = x + (sum xs) 

data Color = Red
           | Blue
           | Green
           | Orange
           -- etc....

data List a = Cons a (List a)
            | Nil

 -- user id, username password
data User = User Int String String 

mylist = Cons 1 $ Cons 2 $ Cons 5 Nil

sumList Nil = 0
sumList (Cons x xs) = x + (sumList xs)

table = zip [3, 2, 5, 6] ["a", "blue", "car", "red"]
-- a = f 5 (f 5 6)

--printResult x = checkForError $ lookup x table
--  where
--    checkForError (Just x) = show x
--    checkForError Nothing = ""

auser = User 0 "job" "mypassword"


data Maybe a = Just a
             | Nothing
  deriving (Show, Eq, Ord, Read)

--test = g (Just 5)

--g (Just x) = x
--g (Nothing) = 0

--class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  return x = Just x
  (Just x) >>= f = f x
  Nothing >>= f = Nothing


addTwo x = return $ x + 2
addThree x = x + 3
addTwoIfLessThan10 x | x < 10 = addTwo x
addTwoIfLessThan10 x = Nothing
 
--test = (Just 4) >>= addTwo >>= (return . addThree) >>= addTwo >>= addTwoIfLessThan10
-- input :: String
-- input :: RealWord -> (RealWorld, String) 
-- readFile :: String -> IO String
main = do
   a <- readFile "somefile.txt" 
   putStr a
   



