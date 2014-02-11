module Main where
import System.Environment (getArgs)
import Data.Bits (testBit)

processLine :: String -> String
processLine l = case map (\x -> (read x::Int)) $ wordsWhen (== ',') l of
	[n, p1, p2] -> boolToString $ sameBit n (p1-1) (p2-1)
	otherwise -> "Invalid input"

boolToString :: Bool -> String
boolToString True = "true"
boolToString False = "false"

-- check if the p1-th bit is the same as the p2-th bit of number n
sameBit :: Int -> Int -> Int -> Bool
sameBit n p1 p2 = bitOfN p1 == bitOfN p2
	where bitOfN = testBit n

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
