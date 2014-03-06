module Main where
import System.Environment (getArgs)
import Data.Char (toUpper, toLower, isUpper)

swapCase :: Char -> Char
swapCase c
	| isUpper c = toLower c
	| otherwise = toUpper c

processLine :: String -> String
processLine = unwords . map (map swapCase) . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
