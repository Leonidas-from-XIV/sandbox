module Main where
import System.Environment (getArgs)

rotations :: String -> [String]
rotations s = rotations' (n-1) s
        where rotations' 0 s = [s]
              rotations' n (s:sx) = rotated:rotations' (n-1) rotated
                      where rotated = sx++[s]
              n = length s

processLine :: String -> String
processLine l = case wordsWhen (== ',') l of
	[a,b] -> if b `elem` rotations a then "True" else "False"
	otherwise -> "Invalid input"

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
	"" -> []
	s' -> w : wordsWhen p s''
		where (w, s'') = break p s'

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input

