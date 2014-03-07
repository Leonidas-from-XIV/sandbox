module Main where
import System.Environment (getArgs)

access :: [a] -> Int -> Maybe a
[] `access` _ = Nothing
(x:_) `access` 1 = Just x
(_:xs) `access` n = xs `access` (n-1)

processLine :: String -> String
processLine l = case reverse $ words l of
	(index:lat) -> case lat `access` (read index::Int) of
		Nothing -> ""
		Just x -> x
	_ -> "Invalid input"

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
