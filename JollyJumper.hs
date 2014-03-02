module Main where
import System.Environment (getArgs)
import Data.List (sort)

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (a:b:xs) = abs (a - b):differences (b:xs)

increasing :: [Int] -> Bool
increasing [] = True
increasing [_] = True
increasing (a:b:xs) = (a+1) == b && increasing (b:xs)

jolly :: [Int] -> Bool
jolly lat = head seq == 1 && increasing seq
	where seq = (sort . differences) lat

showJolly :: Bool -> String
showJolly True = "Jolly"
showJolly False = "Not jolly"

processLine :: String -> String
processLine = showJolly . jolly . tail . map (\x -> read x::Int) . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
