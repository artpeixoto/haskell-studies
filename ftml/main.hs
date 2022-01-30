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


                                                
beginsWith word@(x:xs) text@(y:ys)
  | (length text) < (len word) = False
  | otherwise 	= (all (\x -> fst x == snd x) $ zip word text)

findAll pattern str = map (beginsWith pattern) (scanr (\x y -> x:y) "" str)

parseHtml :: String
	  -> TMLItem

parseHtml = undefined

splitAll :: (String -> Bool) 
         -> String 
         -> [String]
splitAll _ ""   =  "" : [] 
splitAll f xs 	=  fstx ++ (splitAll f tailx) 
		where
                  (fstx, sndx) = break f [xs]
		  (separator, [tailx]) = break (not . f) sndx 

separateCommands 	:: String
                        -> [String]
separateCommands 	=  undefined


sepIdentation :: [Char] -> [Char]
sepIdentation (x:xs) =
	case (isSpace x) of
		  True 	-> x : (sepIdentation xs)
		  False -> []

main = do
	example <- readFile "example.ftml"
	putStrLn $ "trying to parse \n" ++  example
