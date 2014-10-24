import Ladder.Graph
import System.Environment
import System.Exit
import System.IO(hPutStrLn,stderr)
import Data.Char(isAlpha,toLower)

main :: IO ()
main = 
  getArgs >>= parse >>= \r -> putStrLn r

parse :: [String] -> IO String
parse [file, start, finish] = do
  check_length start finish
  dict <- loadDict file (length start)
  check_madeup dict start finish
  case shortestPath (buildGraph dict) start finish of
    Nothing -> return $ "No ladder between " ++ start ++ " and " ++ finish
    Just l  -> return $ show $ Ladder (start:l)
parse _ =
  putStrLn "usage: ladder dict-file-path start-word end-word" >> exitWith ExitSuccess

check_length :: String -> String -> IO ()
check_length a b = if (length a == length b) then return ()
                   else hPutStrLn stderr "Words must be of the same length!" >> exitWith (ExitFailure 1)

check_madeup :: [String] -> String -> String -> IO ()
check_madeup dict a b = if (a `elem` dict && b `elem` dict) then return ()
                        else hPutStrLn stderr "Can't link made-up words!" >> exitWith (ExitFailure 1)

loadDict :: FilePath -> Int -> IO [String]
loadDict path n = readFile path >>= \txt -> return $ map (map toLower) $ filter (\w -> length w == n && all isAlpha w) $ words txt

