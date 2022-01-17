{-# LANGUAGE LambdaCase #-}

import Data.List (elemIndex, maximumBy)
import Data.Ord (comparing)

data Square = Grey Char | Y Char Int | Grn Char Int
    deriving (Show, Eq)

-- The real lists used in the game according to 538. There are many more allowed guesses than secrets
secretList :: IO [String]
secretList = lines <$> readFile "Wordle _ Mystery Words - Sheet1.txt"

guessList :: IO [String]
guessList = lines <$> readFile "Wordle _ Guessable Words.txt"

-- Bool representing whether the given string falls in the subspace defined by the given squares
consistent :: [Square] -> String -> Bool
consistent [] w = True
consistent (x:xs) w =
    case x of
        Grey l -> consistent xs w && notElem l w
        Y l pos -> consistent xs w && elem l w && w!!pos /= l
        Grn l pos -> consistent xs w && w!!pos == l

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

-- The particular response that corresponds to a query-secret pair
response :: String -> String -> [Square]
response query secret = zipWith (\ c n
  -> (if secret !! n == c then
          Grn c n
      else
          case elemIndex c secret of
                Nothing -> Grey c
                _ -> Y c n)) 
    query [0..length query]

-- Returns the optimal sequence of queries for the given secret, set of allowed guesses, and allowed secrets
querySeq :: String -> [String] -> [String] -> [String]
querySeq secret guessSpace [s] = [s]
querySeq secret guessSpace secretSpace = next : querySeq secret guessSpace (filter (consistent $ response next secret) secretSpace)
                                            where next = maximumBy (comparing (`guessEntropy` secretSpace)) guessSpace


main :: IO ()
main = do
    -- hard-coding the first guess (as "raise") makes the computation finish in a reasonable amount of time
    secretSpace <- consistentSecrets $ response "raise" "solar"
    guessSpace <- guessList
    print $ "raise" : querySeq "solar" guessSpace secretSpace