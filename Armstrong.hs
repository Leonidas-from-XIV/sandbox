module Main where
import System.Environment (getArgs)
import Data.Char (digitToInt)

isArmstrong :: Int -> [Int] -> Bool
isArmstrong num digits = num == sum powers
	where powers = map (^n) digits
              n = length digits

showBool :: Bool -> String
showBool True = "True"
showBool _ = "False"

processLine :: String -> String
processLine l = showBool $ isArmstrong number digits
	where number = read l::Int
              digits = map digitToInt l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
