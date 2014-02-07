module Main where
	import System.Environment (getArgs)

	nextFib :: (Int,Int) -> (Int,Int)
	nextFib (old,new) = (new,old+new)

	allFibonacci = iterate nextFib (1,0)

	fib :: Int -> Int
	fib n = case allFibonacci !! n of
		(a, b) -> b

	processLine :: String -> String
	processLine l = show $ fib $ (read l::Int)

	main :: IO()
	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
