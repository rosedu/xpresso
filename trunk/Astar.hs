{- Astar generic algorithm -}

module Astar(
    astar
    )where

import qualified PriorityQueue as Q
import qualified Data.Set as S
import Control.Monad

import Debug.Trace

{--
 - astar :: Start -> (Succ) -> End -> Cost -> Heur
 -}
--astar :: Ord k => a -> (a -> [a]) -> a -> (a -> k) -> (a -> k) -> Maybe b
astar start succ end cost heur
    = astar' (S.singleton start) (Q.singleton (heur start) [start])
    where
	astar' seen q
	    | Q.empty q = error "No solution found!!"
	    | end n = next
	    | otherwise = astar' seen' q'
	    where
		asf (Just x) = x
		asf Nothing = error "No solution found!!!"
		((c, next), dq) = asf $ Q.deleteFindMin q
		n = head next
		succs = filter (`S.notMember` seen) $ succ n
		costs = map ((+c) . (subtract $ heur n) . liftM2 (+) cost heur) succs
		q' = dq `Q.union` Q.fromList (zip costs (map (:next) succs))
		seen' = seen `S.union` S.fromList succs
