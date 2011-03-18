import System.Process
import qualified Data.ByteString.Lazy.Char8 as B8
import System.Environment.XDG.BaseDir
import qualified Data.Map as M
import Data.List

main = do
  whistoryfile <- getUserDataFile "uzbl" "whistory"
  historyfile <- getUserDataFile "uzbl" "history"
  history <- (map (head . drop 2 . B8.words) . B8.lines) `fmap` B8.readFile historyfile
  weighted <- read `fmap` readFile whistoryfile :: IO (M.Map B8.ByteString Float)
  let updatedmap = foldl' (\m u -> M.insertWith' (flip const) u 1 m) weighted history
  mapM_ B8.putStrLn $ map fst $ sortBy (\(_,a) (_,b) -> a `compare` b) $ M.assocs updatedmap
