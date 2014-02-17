module Main where
import System.Environment (getArgs)

fizzBuzz :: Int -> Int -> Int -> String
fizzBuzz a b x
	| a `divides` x && b `divides` x = "FB"
	| a `divides` x = "F"
	| b `divides` x = "B"
	| otherwise = show x
	where divides a b = b `mod` a == 0

processLine :: String -> String
processLine l = case map (\x -> (read x::Int)) $ words l of
	[a,b,n] -> unwords $ map fixbuzz [1..n]
		where fixbuzz = fizzBuzz a b
	otherwise -> "Invalid input"

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
