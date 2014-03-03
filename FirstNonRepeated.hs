module Main where
import System.Environment (getArgs)
import Data.List (groupBy, minimumBy, sortBy)
import Data.Function (on)

nonRepeated :: String -> Char
nonRepeated = snd . minimumBy (compare `on` fst) . map head . filter ((== 1) . length) . groupBy ((==) `on` snd) . sortBy (compare `on` snd) . zip [0..]

processLine :: String -> String
processLine l = [nonRepeated l]

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
