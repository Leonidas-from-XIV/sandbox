module Main where
import System.Environment (getArgs)
import Data.List (groupBy, minimumBy, sortBy)
import Data.Function (on)

nonRepeated :: String -> Char
nonRepeated = snd . minimumBy (compare `on` fst) . map head . filter (\e -> length e == 1) . groupBy (\(_,a) (_,b) -> a == b) . sortBy (compare `on` snd) . zip [0..]

processLine :: String -> String
processLine l = [nonRepeated l]

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
