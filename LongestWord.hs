module Main where
	import System.Environment (getArgs)

	longer :: String -> String -> String
	longer a b = case length b > length a of
		True -> b
		False -> a

	processLine :: String -> String
	processLine l = foldl longer "" $ words l

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
