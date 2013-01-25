import qualified Control.Monad.Random as MR
import Text.JSON
import qualified Data.Map as Dm

fromJSRational (s, (JSRational _ r)) = (s,fromRational r)

ngramp ngJSON = let (Ok (JSObject result)) = decode ngJSON :: Result JSValue
                    in Dm.fromList $ map fromJSRational $ fromJSObject $ result

frequencyMap [] ngmap = ngmap
frequencyMap ((ng, p):ngs) ngmap = case Dm.lookup (head ng) ngmap of
                                Nothing -> frequencyMap ngs (Dm.insert (head ng) [(ng, p)] ngmap)
                                Just xs -> frequencyMap ngs (Dm.insert (head ng) ((ng, p) : xs) ngmap)


generateText n acc cur freqmap = do
                        let (Just probabilities) = Dm.lookup cur freqmap
                        choice <- MR.fromList probabilities
                        let next = (head $ tail choice)
                        case n of 
                            0 -> return acc
                            _ -> generateText (n-1) (acc++([head choice])) next freqmap

gibberish n = do
        js <- readFile "./ngrams.json"
        let pmap = ngramp js
        result <- generateText n [] 't' (frequencyMap (Dm.toList pmap) (Dm.singleton '' []))
        return result