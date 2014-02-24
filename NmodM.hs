module Main where
import System.Environment (getArgs)

myMod n m
	| n < m = n
	| otherwise = myMod (n-m) m

processLine :: String -> String
processLine l = show $ n `myMod` m
	where (n,m) = (read ("(" ++ l ++ ")")::(Int,Int))

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
