import Ladder.Graph
import System.Environment
import System.Exit
import System.IO(hPutStrLn,stderr)

main :: IO ()
main = 
  getArgs >>= parse >>= \r -> putStrLn r

parse :: [String] -> IO String
parse [file, start, finish] = 
  loadDict file (length start) >>= 
    \dict -> if (length start == length finish)
             then case shortestPath (buildGraph dict) start finish of
                    Nothing -> return $ "No ladder between " ++ start ++ " and " ++ finish
                    Just l  -> return $ show $ Ladder (start:l)
             else hPutStrLn stderr "Words must be of the same length!" >> exitWith (ExitFailure 1)
parse _ =
  putStrLn "usage: ladder dict-file-path start-word end-word" >> exitWith (ExitFailure 1)
