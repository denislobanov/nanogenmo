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
    s <- makeSentences $ T.words f
    print s

-- Returns a list representing a sentence from a list of all words
splitToSentence :: ([[Text]], [Text]) -> Text -> ([[Text]], [Text])
splitToSentence a x =
    let l = fst a
        s = snd a ++ [x]
    in if T.any (=='.') x
    then
        -- l is a list of lists
        return ([s]:l, [])
    else
        return (l, s)

-- Create a list of sentences from list of words
makeSentences :: [Text] -> [[Text]]
makeSentences t = do
    r <- foldl splitToSentence ([[]], []) x
    l = fst r
    s = snd r
    if s == []
    then
        return l
    else
        return [s]:l

-- Gives valid Word either from map or new
giveWord :: String -> WordMap -> Word
giveWord k m = do
    let d = Word {value=k,count=0,next=[]}
    return M.lookupDefault d k m

-- Add word to hash map if its not already there
startChain :: Maybe [Text] -> WordMap -> WordMap
startChain Nothing = Nothing
startChain (Just []) m = m
startChain (Just [x:xs]) m = do
    k <- unpack x
    v <- giveWord x m
    let v' = Word {value=k,count = (count v)+1, next=(next v)}
    let m' = M.insert k v' m


