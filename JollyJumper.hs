module Main where
import System.Environment (getArgs)
import Data.List (sort)

differences [] = []
differences [x] = []
differences (a:b:xs) = abs (a - b):differences (b:xs)

jolly :: [Int] -> Bool
jolly [x] = True
jolly lat = [1..(n-1)] == (sort . differences) lat
	where n = length lat

showJolly True = "Jolly"
showJolly False = "Not jolly"

processLine :: String -> String
processLine = showJolly . jolly . tail . map (\x -> read x::Int) . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
