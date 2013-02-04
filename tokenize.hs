import Data.List.Split
import Data.Char
import qualified Data.Map as Dm
import Control.Applicative
import Control.Monad

ngrams' n len xs = let next = (take n xs)
                in case len == n of
                    False -> (toLower <$> next) : (ngrams' n (len - 1) $ drop 1 xs)
                    _     -> return xs
ngrams n xs = ngrams' n (length xs) xs

digrams = ngrams 2
trigrams = ngrams 3
quadgrams = ngrams 4
quintgrams = ngrams 5
-- start out by choosing a random unigram

-- then choose another letter with the conditional probability that it follows the first one
-- will have to calculate the conditional probability that any letter follows any other letter

-- see how many digrams start with that letter
startsP' :: Char -> [String] -> (Int, Int)
startsP' letter dgs = foldr check (0, 0) dgs where
    check (first:second:[]) (a, n) = case (first == letter) of
                                True -> (a + 1, n + 1)
                                _    -> (a, n + 1)
    check (first:[]) (a, n) = (a, n + 1)

startsP letter dgs = let (n, k) = startsP' letter (digrams dgs) 
                        in (fromIntegral n) / (fromIntegral k)


select [] = []
select (x : xs) = (x, xs) : map (fmap (x :)) (select xs)

perm2 ks = [[x,y] | (x, ys) <- select ks, y <- ys]

permute 1 xs = map (:[]) xs
permute 2 xs = perm2 xs
permute n xs = join [[x : p | p <- (permute (n-1) ys)] | (x,ys) <- select xs]

alphabet = ".,\" :;?![]()" ++ ['a'..'z']

repeatLetters n = map $ take n . repeat

englishDigrams = (repeatLetters 2 alphabet) ++ permute 2 alphabet

englishTrigrams = (repeatLetters 3 alphabet) ++ permute 3 alphabet

englishQuadgrams = (repeatLetters 4 alphabet) ++ permute 4 alphabet

englishQuintgrams = (repeatLetters 5 alphabet) ++ permute 5 alphabet

out fname n (d, k) = appendFile fname $ (show d) ++ ":" ++ (show $ k/n) ++ ","

-- ngramProbs :: (Num a, Num t, Ord k) => t -> Dm.Map k a -> [k] -> (t, Dm.Map k a)
-- first argument is all possible ngrams in a Map
-- second argument is all of the tokenized ngrams from the corpus
ngramProbs k ngrams [] = (k, ngrams)
ngramProbs k ngrams (n:ns) = case (Dm.lookup n ngrams) of
                                Nothing -> ngramProbs k ngrams ns
                                (Just count) -> let ngrams' = Dm.insert n (count+1) ngrams
                                                    in ngramProbs (k+1) ngrams' ns

-- buildProbabilities :: (Fractional a, Ord k) => [k] -> Dm.Map k a
buildProbabilities ngrams = Dm.fromList [(ngram, 0) | ngram <- ngrams]

main = do
    corpus <- readFile "./xab"
    let (n, ngramMap) = ngramProbs 0 (buildProbabilities englishQuintgrams) (quintgrams corpus)
    appendFile "./quadgrams.json" "{"
    mapM_ (out "./quadgrams.json" n) $ [(d,k) | (d,k) <- Dm.toList ngramMap, k /= 0]
    appendFile "./quadgrams.json" "}"