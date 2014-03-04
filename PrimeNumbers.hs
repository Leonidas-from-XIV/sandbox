module Main where
import System.Environment (getArgs)
import Data.List (intercalate)

multiple :: Int -> Int -> Bool
multiple of_ candidate = (candidate `rem` of_) == 0

sieve :: Int -> [Int] -> [Int]
sieve p [] = []
sieve p candidates = let x:xs = filter (not . (multiple p)) candidates in
	p:(sieve x xs)

processLine :: String -> String
processLine l = intercalate "," $ map show $ takeWhile (<n) $ sieve 2 [2..]
	where n = read l::Int

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
