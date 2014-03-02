module Main where
import System.Environment (getArgs)
import Data.List (sort)

differences :: [Int] -> [Int]
differences xs = map abs $ zipWith subtract xs (tail xs)

increasing :: [Int] -> Bool
increasing xs = all (== True) $ zipWith (\a b -> a+1 == b) xs (tail xs)

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
