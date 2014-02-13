module Main where
import System.Environment (getArgs)
import Data.List (sortBy)

lineCollector :: Int -> [String] -> String -> [String]
lineCollector 0 [] line = []
lineCollector n [] line = [line]
lineCollector n (smallest:xs) line = if n > l then (if length line < length smallest then line:smallest:xs else smallest:line:xs)
	else let (a:b) = sortBy (\a b -> compare (length a) (length b)) (smallest:xs)
	in (if length line < length a then a else line):b
	where l = length (smallest:xs)

processLine :: [String] -> [String]
processLine (n:content) = foldl collector [] content
	where collector = lineCollector (read n::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ processLine $ lines input
