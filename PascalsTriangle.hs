module Main where
import System.Environment (getArgs)

slidingWindow :: [Int] -> [[Int]]
slidingWindow [] = []
slidingWindow [a] = []
slidingWindow (a:b:xs) = [a,b]:(slidingWindow (b:xs))

pascalTriangle :: Int -> [Int]
pascalTriangle 0 = []
pascalTriangle 1 = [1]
pascalTriangle n = 1:(map sum $ slidingWindow $ pascalTriangle $ n-1) ++ [1]

processLine :: String -> String
processLine l = unwords $ map show $ concatMap pascalTriangle [1..n]
	where n = (read l::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
