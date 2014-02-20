module Main where
import System.Environment (getArgs)

addPossibility :: [[Int]] -> [[Int]]
addPossibility acc = acc >>= (\tail -> map (:tail) [1,2])

possibilities = iterate addPossibility [[1], [2]]

-- man, this is inefficient
validPossibilities :: Int -> [[Int]]
validPossibilities n = filter (\e -> sum e == n) $ concat $ take n possibilities

processLine :: String -> String
processLine l = show $ length $ validPossibilities n
	where n = (read l::Int)

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
