module Main where
	import Data.List (find,)

	candidates = [2..1000]

	multiple :: Integer -> Integer -> Bool
	multiple of_ candidate = (candidate `rem` of_) == 0

	sieve :: Integer -> [Integer] -> [Integer]
	sieve p [] = []
	sieve p candidates = let x:xs = filter (\x -> not (multiple p x)) candidates in
		p:(sieve x xs)

	palindrome :: [Char] -> Bool
	palindrome x = reverse x == x

	main :: IO()
	main = case find palindrome (map show (reverse (sieve 2 candidates))) of
		Just x -> putStrLn x
		otherwise -> putStrLn "No solution"
