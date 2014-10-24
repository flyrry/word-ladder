import Ladder.Graph
import System.Environment
import System.Exit
import Data.Char(isAlpha,toLower)
import Control.Monad(unless)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, start, finish] -> do
      unless (length start == length finish) $ error "Words must be of the same length!"
      dict <- loadDict file (length start)
      unless (start `elem` dict && finish `elem` dict) $ error "Can't link made-up words!"
      case shortestPath (buildGraph dict) start finish of
        Nothing -> putStrLn $ "No ladder between " ++ start ++ " and " ++ finish
        Just l  -> putStrLn $ show $ Ladder (start:l)
    _ -> putStrLn "usage: ladder dict-file-path start-word end-word" >> exitWith ExitSuccess

loadDict :: FilePath -> Int -> IO [String]
loadDict path n = readFile path >>= \txt -> return $ map (map toLower) $ filter (\w -> length w == n && all isAlpha w) $ words txt

