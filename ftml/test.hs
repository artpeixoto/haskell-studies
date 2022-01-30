import Control.Monad
import Data.Char

import qualified Data.Text as T


group':: Eq a => a -> [a] -> [[a]]
group' _  [] = [[]]
group' a' (x:xs) 
	| (x == a')		= (a' : (head next)) : (tail next)  
	| (x /= a')  	= (a' : []) : next 
	where 
			next = group' x xs 

splitWord :: String 
					-> String
					-> [String]
splitWord = undefined 