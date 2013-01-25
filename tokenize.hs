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

permute 2 xs = perm2 xs
permute n xs = join [[x : p | p <- (permute (n-1) ys)] | (x,ys) <- select xs]

alphabet = ((map return ".,\" :;?![]()") ++ (map return ['a'..'z'])) ++ (map (\x -> [x,x]) ['a'..'z'])

englishDigrams = permute 2 alphabet

englishTrigrams = permute 3 alphabet

out fname n (d, k) = appendFile fname $ (show d) ++ ":" ++ (show $ k/n) ++ ","

digramProbs :: (Num a, Num t, Ord k) => t -> Dm.Map k a -> [k] -> (t, Dm.Map k a)
-- first argument is tokenized digrams
-- seconds argument is all possible digrams
digramProbs n ds [] = (n, ds)
digramProbs n digrams (d:ds) = case (Dm.lookup d digrams) of
                                Nothing -> digramProbs n digrams ds
                                (Just count) -> let digrams' = Dm.insert d (count+1) digrams
                                                    in digramProbs (n+1) digrams' ds

buildProbabilities :: (Fractional a, Ord k) => [k] -> Dm.Map k a
buildProbabilities digrams = Dm.fromList [(digram, 0.0) | digram <- digrams]

-- main = do
    -- corpus <- readFile "./xab"
    -- let (n, digramMap) = digramProbs 0 (buildProbabilities englishTrigrams) (trigrams corpus)
    -- mapM_ (out "./test2.json" n) $ Dm.toList digramMap
    