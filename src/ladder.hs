import Ladder.Graph
import System.Environment
import System.Exit
import System.IO(hPutStrLn,stderr)
import Data.Char(isAlpha,toLower)

main :: IO ()
main = 
  getArgs >>= parse >>= \r -> putStrLn r

parse :: [String] -> IO String
parse [file, start, finish] = 
  if (length start == length finish)
  then loadDict file (length start) >>= \dict ->
        if (start `notElem` dict || finish `notElem` dict)
        then hPutStrLn stderr "Can't link made-up words!" >> exitWith (ExitFailure 1)
        else case shortestPath (buildGraph dict) start finish of
              Nothing -> return $ "No ladder between " ++ start ++ " and " ++ finish
              Just l  -> return $ show $ Ladder (start:l)
  else hPutStrLn stderr "Words must be of the same length!" >> exitWith (ExitFailure 1)
parse _ =
  putStrLn "usage: ladder dict-file-path start-word end-word" >> exitWith ExitSuccess

loadDict :: FilePath -> Int -> IO [String]
loadDict path n = readFile path >>= \txt -> return $ map (map toLower) $ filter (\w -> length w == n && all isAlpha w) $ words txt

