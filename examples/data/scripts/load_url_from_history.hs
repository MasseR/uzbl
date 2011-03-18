import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment.XDG.BaseDir
import qualified Data.Map as M
import Data.List
import System.Environment

calculatedHistory :: IO (M.Map B8.ByteString Float)
calculatedHistory = do
  whistoryfile <- getUserDataFile "uzbl" "whistory"
  h <- (map (B8.words) . B8.lines) `fmap` B8.readFile whistoryfile
  return $ foldr (\[w, u] m -> M.insertWith' (\_ _ -> undefined) u ((read . B8.unpack) w) m) M.empty h

main = do
  weighted <- calculatedHistory
  historyfile <- getUserDataFile "uzbl" "history"
  history <- (map (head . drop 2 . B8.words) . B8.lines) `fmap` B8.readFile historyfile
  let updatedmap = foldr (\u m -> M.insertWith' (flip const) u 0.5 m) weighted history
      urls = map fst $ sortBy (\(_,a) (_,b) -> b `compare` a) $ M.assocs updatedmap
  B8.putStr $ B8.unlines urls
