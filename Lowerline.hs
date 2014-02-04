module Main where
	import Data.Char (toLower)
	import System.Environment (getArgs)

	processLine :: String -> String
	processLine l = map toLower l

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
