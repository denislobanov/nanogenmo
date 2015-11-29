{-# LANGUAGE OverloadedStrings #-}

import Data.LinkedHashMap.Seq (LinkedHashMap)
import qualified Data.LinkedHashMap.Seq as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L

-- just in case, hopefully wont need it
data Pair = Pair (String, Int)

type WordMap = Maybe LinkedHashMap
data Word = Word { value :: String
                 , count :: Integer
                 , next :: [(String, Int)]
                 } deriving (Show)

main :: IO ()
main = do
    f <- TIO.readFile "source.text"
    r <- parseSentences $ T.words f

-- Returns a list representing a sentence from a list of all words
splitToSentence :: [Text] -> Maybe [Text]
splitToSentence [] = Nothing
splitToSentence = L.dropWhileEnd (\x -> T.any (=='.') x)

-- Add word to hash map if its not already there
startChain :: Maybe [Text] -> WordMap -> WordMap
startChain Nothing = Nothing
startChain (Just []) m = m
startChain (Just [x:xs]) m = do
    k <- unpack x
    let d = Word {value=x,count=0,next=[]}
    v <- M.lookupDefault d k m
    let v' = Word {value=x,count = (count v)+1, next=(next v)}
    let m' = M.insert k v' m


