import Control.Monad
import Data.Char

import qualified Data.Text as T


group' :: Eq b => (a -> b) -> [a] -> [[a]]
group' f (x:xs) = groupInner f x (xs)
 	where
		groupInner :: Eq b1 => (a -> b1) -> a	-> [a]	-> [[a]]
		groupInner f' a'  [] = [[a']]
		groupInner f' a' (x':xs') 
			| (fx == fa)		= (a' : (head next)) : (tail next)  
			| (fx /= fa)  	= (a' : []) : next 
			where 
				fx = f' x'
				fa = f' a'
				next = groupInner f' x' xs'

splitWord :: String 
					-> String
					-> [String]
splitWord = undefined 