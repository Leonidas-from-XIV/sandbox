module Main where
	main :: IO()
	main = mapM_ putStrLn $ map show $ [1,3..99]
