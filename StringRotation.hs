module Main where
import System.Environment (getArgs)

rotations 0 s = [s]
rotations n (s:sx) = rotated:rotations (n-1) rotated
	where rotated = sx++[s]

isRotation a b = if b `elem` rotations (n-1) a then "True" else "False"
	where n = length a

processLine :: String -> String
processLine l = case wordsWhen (== ',') l of
	[a,b] -> isRotation a b
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

