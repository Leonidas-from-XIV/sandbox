module Main where
import System.Environment (getArgs)

eval :: [Double] -> String -> [Double]
eval (a:b:xs) "+" = (a+b):xs
eval (a:b:xs) "-" = (a-b):xs
eval (a:b:xs) "*" = (a*b):xs
eval (a:b:xs) "/" = (a / b):xs
eval xs num = (read num::Double):xs

processLine :: String -> String
processLine = show . truncate . head . foldl eval [] . reverse . words

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
