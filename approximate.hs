{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Control.Monad.Random as MR
import Data.Aeson
import Data.Aeson.Types
import Data.Map as Dm
import qualified Data.ByteString.Lazy as DB
import Data.Char

frequencyMap [] ngmap = ngmap
frequencyMap ((_, 0.0):ngs) ngmap = frequencyMap ngs ngmap
frequencyMap ((ng, p):ngs) ngmap = case Dm.lookup (DB.head ng) ngmap of
                                Nothing -> frequencyMap ngs (Dm.insert (DB.head ng) [(ng, p)] ngmap)
                                Just xs -> frequencyMap ngs (Dm.insert (DB.head ng) ((ng, p) : xs) ngmap)

nextChoice k xs = case k of
                    1 -> DB.pack [DB.head xs]
                    _ -> DB.take (k-1) xs


generateText k n acc cur freqmap = do
                        let (Just probabilities) = Dm.lookup cur freqmap
                        choice <- MR.fromList probabilities
                        let next = DB.last choice
                        case n of 
                            0 -> return acc
                            _ -> generateText k (n-1) (DB.append acc (nextChoice k choice)) next freqmap

gibberish n = do
        js <- (DB.readFile "./quadgrams.json")
        let (Just ngramPairs) = decode js :: Maybe (Dm.Map DB.ByteString Rational)
        result <- generateText 4 n "" 116 (frequencyMap (Dm.toList ngramPairs) (Dm.singleton 0 []))
        return result

main = do
        output <- gibberish 1550
        DB.writeFile "./output3.txt" output