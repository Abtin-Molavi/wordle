{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (elemIndex, maximumBy, elemIndices)
import Data.Ord (comparing)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(withDebugInfo))


data Square = Grey Char | Y Char Int | Grn Char Int
    deriving (Show, Eq)

-- The real lists used in the game according to 538. There are many more allowed guesses than secrets
secretList :: IO [String]
secretList = lines <$> readFile "Wordle _ Mystery Words - Sheet1.txt"

guessList :: IO [String]
guessList = lines <$> readFile "Wordle _ Guessable Words.txt"

countSq :: Char -> [Square] -> Int
countSq c [] = 0
countSq c (Y l n:sqs) | l==c = 1 + countSq c sqs
countSq c (Grn l n:sqs) | l==c = 1 + countSq c sqs
countSq c (x:xs) = countSq c xs

count :: (Eq a) => a -> [a] -> Int
count elem l = length (filter (== elem) l)

positionsConsistent :: [Square] -> String  -> Bool 
positionsConsistent [] guess = True 
positionsConsistent (Grn c n:sqs) guess = positionsConsistent sqs guess && n `elem` elemIndices c guess
positionsConsistent (sq:sqs) guess = positionsConsistent sqs guess



-- Bool representing whether the given string falls in the subspace defined by the given squares
consistent :: [Square] -> String -> Bool
consistent sqs guess = all (\c -> (countSq c sqs == count c guess || (countSq c sqs < count c guess && numGrey c sqs == 0)) && positionsConsistent sqs guess) (fromSqs sqs)
                        where 
                              numGrey c [] = 0
                              numGrey c ((Grey l):sqs) | l == c = 1 + numGrey c sqs
                              numGrey c (sq:sqs) = numGrey c sqs
                              countGuess c = length (filter (== c) guess)
                              fromSqs = map (\case
                                                    (Grn c n) -> c
                                                    (Y c n) -> c
                                                    (Grey c) -> c)

consistentSecrets :: [Square] -> IO [String]
consistentSecrets sqs = filter (consistent sqs) <$> secretList

-- All of the possible responses for the given query
responses :: String -> [[Square]]
responses [] = [[]]
responses (c:cs) = [Grey c:resp | resp <- map modGreens (responses cs)] ++ [Y c 0:resp | resp <- map modGreens (responses cs)] ++ [Grn c 0:resp | resp <- map modGreens (responses cs)]
    where modGreens = map (\case
                                    (Grn c n) -> Grn c (n+1)
                                    (Y c n) -> Y c (n+1)
                                    x -> x)

-- The Shannon entropy of the random variable corresponding to the response to the given query
guessEntropy :: String -> [String] -> Double
guessEntropy guess secretSpace =  - sum [p*log' p | p <-  probList]
    where probList = map (\x -> fromIntegral (length (consistentSet x)) / fromIntegral (length secretSpace)) (responses guess)
          consistentSet response = filter (consistent response) secretSpace
          log' x = if x == 0 then 0 else log x

greens :: Char -> String -> String  -> Int
greens c [] _ = 0
greens c _ [] = 0
greens c (x:xs) (y:ys) | y == x && x == c = 1 + greens c xs ys
greens c (x:xs) (y:ys)= greens c xs ys



-- The particular response that corresponds to a query-secret pair
response :: String -> String -> [Square]
response query secret = zipWith (\ c n
  -> (if secret !! n == c then
          Grn c n
      else 
          case elemIndex c secret of
                Nothing -> Grey c
                _ -> if count c query > count c secret && n > elemIndices c query !! count c secret - 1
                        then Grey c
                        else Y c n)) 
    query [0..length query]

-- Returns the optimal sequence of queries for the given secret, set of allowed guesses, and allowed secrets
querySeq :: String -> [String] -> [String] -> [String]
querySeq secret guessSpace [] = []
querySeq secret guessSpace [s] = [s]
querySeq secret guessSpace secretSpace = next : querySeq secret guessSpace (filter (consistent $ response next secret) secretSpace)
                                            where bestOverall = maximumBy (comparing (`guessEntropy` secretSpace)) guessSpace
                                                  bestSecret = maximumBy (comparing (`guessEntropy` secretSpace)) secretSpace
                                                  -- break ties such that there's a chance you get lucky
                                                  next = if guessEntropy bestOverall secretSpace > guessEntropy bestSecret secretSpace 
                                                            then bestOverall
                                                            else bestSecret


main :: IO ()
main = do
    [word] <- getArgs
    -- hard-coding the first guess (as "soare") makes the computation finish in a reasonable amount of time
    secretSpace <- consistentSecrets $ response "soare" word
    guessSpace <- guessList
    print $ "soare" : querySeq word guessSpace secretSpace
