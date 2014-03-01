module Main where
import System.Environment (getArgs)
import Data.Char (toUpper)

uppercaseFirst :: String -> String
uppercaseFirst (s:tring) = toUpper s:tring

processLine :: String -> String
processLine = unwords . map uppercaseFirst . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
