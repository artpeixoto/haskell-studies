module Main where
import Control.Monad
import Data.Char

(<|) :: (a -> b) -> a -> b
(<|) = ($)

type Name 		= String
type Literal	= String
data TMLItem 	= TMLLit  Literal 
							| TMLFlag Name 
							| TMLAttr Name 	Literal
							| TMLTag  Name 	[TMLItem]
isAttr (TMLAttr name value) = True 
isAttr _ = False


newtype Html = Html TMLItem
instance Show (Html) where
	show (Html (TMLTag name items)) = 
		"<" ++ name ++ " " ++ itemsAttrs ++ ">" ++
		(foldl (\acc l -> acc ++ "\n\t" ++ l) "") . lines $ restOfItems 
		where
			itemsAttrs = foldl1 (++) . map (\x' -> ' ' : (show . Html $ x')) . filter (isAttr) $ items
			restOfItems = map (
			) 
parseHtml :: String
	  			-> TMLItem

parseHtml = undefined

sepIdentation :: [Char] -> [Char]
sepIdentation (x:xs) =
	case (isSpace x) of
		  True 	-> x : (sepIdentation xs)
		  False -> []

main = do
	example <- readFile "example.ftml"
	putStrLn $ "trying to parse \n" ++  example
