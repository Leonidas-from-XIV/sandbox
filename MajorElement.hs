module Main where
import System.Environment (getArgs)

increment :: [(Int,Int)] -> Int -> ([(Int,Int)], Int)
increment [] a = ([(a, 1)], 1)
increment ((a,b):xs) a'
	| a' == a = ((a, (b+1)):xs, (b+1))
	| otherwise = ((a,b):map, val)
            where (map, val) = increment xs a'

process :: Int -> [Int] -> [(Int,Int)] -> Maybe Int
process req [] _ = Nothing
process req (x:xs) map = if frequency > req then Just x else process req xs map'
	where (map', frequency) = increment map x

showProcess :: Maybe Int -> String
showProcess Nothing = "None"
showProcess (Just x) = show x

processLine :: String -> String
processLine l = showProcess $ process req entries []
	where req = n `quot` 2
	      n = length entries
              entries = read ("[" ++ l ++ "]")::[Int]

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
