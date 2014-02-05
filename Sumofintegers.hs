module Main where
	import System.Environment (getArgs)

	processLine :: String -> Int
	processLine l = read l::Int

	main :: IO()
	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		putStrLn $ show $ sum $ map processLine $ lines input
