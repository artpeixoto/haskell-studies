import Data.Monoid
{--
class Monoid (m a) => Monoider m where
	expandMonoider :: [a]
		       -> m a 
		       -> [m a]
--}

dat Tree a  = EmptyNode
	     | NodeNull	(Tree a) (Tree a) 
	     | Node a 	(Tree a) (Tree a)

( <| ) = ( $ )
( |> ) = \x -> \y -> (y x)

data Direction 	= L
		| R
		| F
		| B
		| U
		| D

