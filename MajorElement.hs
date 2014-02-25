module Main where
import System.Environment (getArgs)
import Data.List (sort, group)

majorElement :: [[Int]] -> String
majorElement [] = "None"
majorElement [(a:_)] = show a

processLine :: String -> String
processLine l = majorElement $ filter (\x -> length x > n `quot` 2) $ group $ sort entries
	where n = length entries
              entries = read ("[" ++ l ++ "]")::[Int]

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
