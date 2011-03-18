import qualified Data.ByteString.Lazy.Char8 as B8
import Data.List
import qualified Data.Map as M
import System.Environment.XDG.BaseDir

sigmoid x = 1 / (exp (-x) + 1)


main :: IO ()
main = do
  whistoryfile <- getUserDataFile "uzbl" "whistory"
  historyfile <- getUserDataFile "uzbl" "history"
  history <- (map (head . drop 2 . B8.words) . B8.lines) `fmap` B8.readFile historyfile
  let n = fromIntegral $ length history :: Float
      (_, initial) = foldr (\u (i, a) -> (succ i, (u, sigmoid (i / n)) : a)) (1, []) history
      grouped = groupBy (\a b -> fst a == fst b) $ sort initial
      weights = map (\x -> (fromIntegral (length x), last x)) grouped
      maxweight = maximum $ map fst weights
      total = map (updateWeights maxweight) weights
      rbmap = M.fromList $ total
  B8.writeFile whistoryfile $ B8.pack $ show rbmap
  where
    updateWeights maxweight (weight, (url, oldweight)) =
      (url, (sigmoid (weight / maxweight)) * oldweight)
