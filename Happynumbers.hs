module Main where
	import System.Environment (getArgs)
	import Data.Char (digitToInt)

	-- well, that's certainly a very happy step
	happyStep :: Int -> Int
	happyStep n = sum $ map (^2) $ map digitToInt $ show n

	happySeq :: Int -> [Int]
	happySeq n = iterate happyStep n

	-- determine if the, possibly infinite, sequence includes a 1
	traverse :: [Int] -> [Int] -> Bool
	traverse seen (x:xs) = case x of
		1 -> True
		otherwise -> case x `elem` seen of
			True -> False
			False -> traverse (x:seen) xs

	processLine :: String -> String
	processLine l = case traverse [] $ happySeq (read l::Int) of
		True -> "1"
		False -> "0"

	main = do
		[inpFile] <- getArgs
		input <- readFile inpFile
		mapM_ putStrLn $ map processLine $ lines input
