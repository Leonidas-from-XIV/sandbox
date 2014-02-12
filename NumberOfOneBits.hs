module Main where
import System.Environment (getArgs)

-- divides n by 2 iteratively until it reaches 0 and checks how many numbers
-- are odd, meaning have a 1 as lowest bit
oneBits :: Int -> Int
oneBits n = length $ filter odd $ takeWhile (/= 0) $ iterate (`div` 2) n

processLine :: String -> String
processLine l = show $ oneBits $ (read l::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
