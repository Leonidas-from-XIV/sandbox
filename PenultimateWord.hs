module Main where
import System.Environment (getArgs)

whichWord :: [String] -> String
whichWord = head . tail . reverse

processLine :: String -> String
processLine = whichWord . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
