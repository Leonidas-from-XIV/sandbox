module Main where
	import System.Environment (getArgs)
	
	processNumber :: Int -> String
	processNumber n = case even n of
		True -> "1"
		False -> "0"

	processLine :: String -> String
	processLine l = processNumber (read l::Int)

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
