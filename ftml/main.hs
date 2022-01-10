module Main where

import Control.Monad
import Data.Char
import Rules

type Name 	= String
type Literal	= String
data TMLItem 	= TMLLit  Literal 
		| TMLFlag Name 
		| TMLAttr Name 	Literal
		| TMLTag  Name 	[TMLItem]

beginsWith xs ys | ygtx == GT 	= False
		 | otherwise 	= (all (\x -> fst x == snd x) (zip xs ys))
		 where		
		 	ygtx = compare (length xs) (length ys) 				

findAll pattern str = map (beginsWith pattern) (scanr (\x y -> x:y) "" str)

parseHtml :: String
	  -> TMLItem
parseHtml = undefined

splitAll  	:: (String -> Bool) -> String -> [String]
splitAll _ ""   =  "" : [] 
splitAll f xs 	=  fstx ++ (splitAll f tailx) 
			where
				(fstx, sndx) = break f [xs]
				(separator, [tailx]) = break (not . f) sndx 
separateCommands :: String
		 -> [String]
separateCommands =  undefined

main :: IO ()
main = do
	example <- readFile "example.ftml"
	putStrLn $ "trying to parse \n" ++  example
