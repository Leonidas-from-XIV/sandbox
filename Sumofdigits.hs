module Main where
	import System.Environment (getArgs)
	import Data.Char (digitToInt)

	processLine :: String -> String
	processLine l = show $ sum $ map digitToInt l

	main :: IO()
	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input

