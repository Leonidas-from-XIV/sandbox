module Main where
import System.Environment (getArgs)
import Data.Ratio ((%), Ratio)

eval :: [Ratio Int] -> [Char] -> [Ratio Int]
eval (a:b:xs) "+" = (a+b):xs
eval (a:b:xs) "-" = (a-b):xs
eval (a:b:xs) "*" = (a*b):xs
eval (a:b:xs) "/" = (a / b):xs
eval xs num = ((read num::Int) % 1):xs

processLine :: String -> String
processLine = show . truncate . head . foldl eval [] . reverse . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
