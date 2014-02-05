module Main where
	import System.Environment (getArgs)

	multiplesOf :: Int -> [Int]
	multiplesOf n = [n,2*n..]

	getBiggerThan x n = case dropWhile (< x) (multiplesOf n) of
		[] -> 0
		(x:xs) -> x
	
	processLine :: String -> String
	processLine l = 
		let params = map (\x -> read x::Int) (wordsWhen (== ',') l)
		in show $ getBiggerThan (params !! 0) (params !! 1)

	wordsWhen :: (Char -> Bool) -> String -> [String]
	wordsWhen p s =  case dropWhile p s of
		"" -> []
		s' -> w : wordsWhen p s''
			where (w, s'') = break p s'

	main :: IO()
	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
