{-# LANGUAGE OverloadedStrings #-}

import Data.LinkedHashMap.Seq
import qualified Data.Text.IO as TIO
import Data.Text

type Words = LinkedHashMap k v
data Word = Words { value :: String,
                  , count :: Integer,
                  , words :: Words
                  } deriving(Show)

type States = LinkedHashMap kv
data States = States {
    value :: String,
    probability :: Float,
    states :: States
} deriving(Show)


main :: IO ()
main = do
    f <- TIO.readFile "source.text"



