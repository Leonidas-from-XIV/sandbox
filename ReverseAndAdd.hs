module Main where
import System.Environment (getArgs)

reverseAndAdd :: Int -> Int
reverseAndAdd n = n + rev
	where rev = read ((reverse . show) n)::Int

reverseAddSeq = iterate reverseAndAdd
stepSeq start = zip [0..] $ reverseAddSeq start

palindrome :: (Int,Int) -> Bool
palindrome (_, n) = sn == reverse sn
	where sn = show n

processLine :: String -> String
processLine l = let ((iteration, res):_) = dropWhile (not . palindrome) $ stepSeq start in
		(show iteration) ++ " " ++ (show res)
	where start = read l::Int

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
