module Main where
import System.Environment (getArgs)

nextFib :: (Integer,Integer) -> (Integer,Integer)
nextFib (old,new) = (new,old+new)

allFibonacci = iterate nextFib (1,0)

fib :: Int -> Integer
fib n = case allFibonacci !! n of
	(a, b) -> b

steps :: Int -> Integer
steps 0 = 0
steps n = fib (n+1)

processLine :: String -> String
processLine l = show $ steps n
	where n = (read l::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
