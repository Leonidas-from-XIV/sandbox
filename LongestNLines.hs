module Main where
import System.Environment (getArgs)
import Data.List (insertBy)
import Data.Function (on)

lineCollector :: Int -> [String] -> String -> [String]
lineCollector 0 [] candidate = []
lineCollector n [] candidate = [candidate]
lineCollector n (smallest:rest) candidate
	| n > length rest + 1 =
		-- the accumulator is not yet filled with n elements
		if length candidate > length smallest then
			smallest:candidate:rest
		else
			candidate:smallest:rest
	| otherwise = tail $ insertBy (compare `on` length) candidate (smallest:rest)

processLine :: [String] -> [String]
processLine (n:content) = reverse $ foldl collector [] content
	where collector = lineCollector (read n::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ processLine $ lines input
