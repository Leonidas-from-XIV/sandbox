module Main where
import System.Environment (getArgs)

possibleDivisions n = [l | l <- [1..n], n `rem` l == 0]

reconstructs :: String -> Int -> Int -> (Int, Bool)
reconstructs this n l = (l, this == (take n $ concat $ repeat chunk))
	where chunk = take l this

processLine :: String -> String
processLine l = show $ fst $ head $ take 1 $ filter snd $ map (reconstructs l n) (possibleDivisions n)
	where n = length l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
