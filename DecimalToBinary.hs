module Main where
import System.Environment (getArgs)
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.Char (intToDigit)

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f x = reverse (unfoldr (fmap swap . f) x)

numToBase :: Int -> Int -> [Int]
numToBase base = unfoldl (\k -> if k == 0 then Nothing else Just (divMod k base))

numToBin :: Int -> [Int]
numToBin 0 = [0]
numToBin n = numToBase 2 n

binToString :: [Int] -> String
binToString xs = map intToDigit xs

processLine :: String -> String
processLine l = binToString $ numToBin (read l::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
