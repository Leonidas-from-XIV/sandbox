module Main where
import System.Environment (getArgs)

--data Rect = Rect {ulx::Int, uly::Int, lrx::Int, lry::Int} 
data Rect = Rect Int Int Int Int

overlap :: Rect -> Rect -> Bool
overlap (Rect aulx auly alrx alry) (Rect bulx buly blrx blry)
	| auly <= buly && auly >= blry && aulx <= bulx && aulx >= blrx = True
	-- … more cases
	| otherwise = False

processLine :: String -> String
processLine l = l

main :: IO ()
main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	mapM_ putStrLn $ map processLine $ lines input
