module Main where
	import System.Environment (getArgs)

	nonempty :: String -> Bool
	nonempty x = x /= ""

	processLines :: [String] -> [String]
	processLines l = map processLine $ filter nonempty l

	processLine :: String -> String
	processLine l = unwords $ reverse $ words l

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ processLines $ lines input
