module Main where
import System.Environment (getArgs)

data Rect = Rect Int Int Int Int
data Point = Point Int Int

contained :: Rect -> Point -> Bool
contained (Rect ulx uly lrx lry) (Point x y)
	| x >= ulx && x <= lrx && y >= lry && y <= uly = True
	| otherwise = False

edges :: Rect -> [Point]
edges (Rect ulx uly lrx lry) = [Point ulx uly, Point ulx lry, Point lrx lry, Point lrx uly]

showBool :: Bool -> String
showBool True = "True"
showBool _ = "False"

processLine :: String -> String
processLine l = case map (\e -> (read e::Int)) $ wordsWhen (== ',') l of
	[a,b,c,d,e,f,g,h] -> showBool $ any (contained (Rect e f g h)) $ edges (Rect a b c d)
	_ -> "Invalid input"

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
