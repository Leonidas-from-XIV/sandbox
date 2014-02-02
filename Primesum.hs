module Main where
	multiple :: Integer -> Integer -> Bool
	multiple of_ candidate = (candidate `rem` of_) == 0

	sieve :: Integer -> [Integer] -> [Integer]
	sieve p [] = []
	sieve p candidates = let x:xs = filter (not . (multiple p)) candidates in
		p:(sieve x xs)

	main :: IO()
	main = putStrLn $ show $ sum $ take 1000 $ sieve 2 [2..]
