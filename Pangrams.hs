module Main where
import System.Environment (getArgs)
import Data.Char (toLower)
import Data.List (sort)

alphabet = ' ':['a'..'z']

missing :: Ord a => [a] -> [a] -> [a]
missing [] _ = []
missing as [] = as
missing (a:as) (p:ps)
	| p == a = missing as ps
	| p < a = missing (a:as) ps
	| p > a = a:missing as (p:ps)

showResult :: String -> String
showResult [] = "NULL"
showResult x = x

processLine :: String -> String
processLine = showResult . missing alphabet . sort . map toLower

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
