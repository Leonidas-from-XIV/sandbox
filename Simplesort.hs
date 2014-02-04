module Main where
	import Data.List (sort)
	import Text.Printf (printf)
	import System.Environment (getArgs)

	processLine :: String -> String
	processLine l = unwords $ map (\x -> printf "%.3f" x) $ sort $ map (\x -> read x :: Float) $ words l

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
