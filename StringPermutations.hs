module Main where
import System.Environment (getArgs)
import Data.List (sort, permutations, intercalate)

processLine :: String -> String
processLine l = intercalate "," $ sort $ permutations l

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
