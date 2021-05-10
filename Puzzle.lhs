> {-# LANGUAGE GADTSyntax        #-}
> module Puzzle where
> import Problems
> import Data.List            as L
> import qualified Data.Map   as M
> import qualified Data.Graph as G

tryProb attempts to execute a given series of
instructions on some problem. If it succeeds,
it shows the final state. Otherwise, it shows
the error that caused it to fail.

> execProb :: Prob -> [[String]] -> IO ()
> execProb p ins = putStrLn (tryProb p ins "")

> tryProb :: Prob -> [[String]] -> String -> String
> tryProb p [] curr       = curr ++ (showProb p)
> tryProb p (l:rest) curr = case tryCross p (L.sort l) of
>   Right newp -> tryProb newp rest (curr ++ (showProb p) ++ "\n")
>   Left err   -> curr ++ (showProb p) ++ "\n" ++ showErr err

> cross :: Prob -> [String] -> Prob
> cross p load = case tryCross p load of
>   Right newp -> newp
>   Left _   -> p

tryCross encapsulates both the validation and
actual execution of some instructions on a prob.
It first runs some surface level tests to see
if some instruction is even valid. If so,
it will start validateCross for the next step of
validation.

> tryCross :: Prob -> [String] -> Either ProbError Prob
> tryCross (Prob m bsize s g (s1, s2)) load
>   | length load > bsize                              = Left (LoadError (length load) bsize)
>   | not (L.all (\x -> L.elem x (M.keys m)) load)     = Left (NoneError (head (load \\ (M.keys m))))
>   | (s == L && not (load `isInfixOf` (L.sort s1)))   = Left (AbsentError (head (load \\ (L.sort s1))))
>   | (s == R && not (load `isInfixOf` (L.sort s2)))   = Left (AbsentError (head (load \\ (L.sort s2))))
>   | otherwise                                        = validateCross (crossedProb (Prob m bsize s g (L.sort s1, L.sort s2)) (L.sort load))
>

validateCross calls a helper function, val, to
test if a given gamestate violates any of the
conflict restraints. That is to say, if someone
tries to leave a wolf and a goat alone together,
this is where it fails.

> validateCross :: Prob -> Either ProbError Prob
> validateCross (Prob m bsize s g (st1, st2)) = case s of
>   L -> val (Prob m bsize s g (st1, st2)) st2
>   R -> val (Prob m bsize s g (st1, st2)) st1

This function actually executes the crossing
of items. We have to match to determine which
side of the river is losing items and which is
gaining, and also to update the current position
of the boat.

> crossedProb :: Prob -> [String] -> Prob
> crossedProb (Prob m bsize s g (y1, y2)) load = case s of
>   L -> Prob m bsize R g (filter (\x -> L.notElem x load) y1, load++y2)
>   R -> Prob m bsize L g (load++y1, filter (\x -> L.notElem x load) y2)

(bad code incoming)
I validate this by finding all possible pairs of items
on the side of the river that the boat has just left.
If any of those pairs are present in the graph's edge
list (that is to say, any two of those vertices have
an edge between them), then we have an invalid configuration.

I think this runs in polynomial time and I'm almost
certain it *could* be done in linear time, but my
Haskell know-how is weak.

> val :: Prob -> [String] -> Either ProbError Prob
> val (Prob m bsize s g state) l = 
>   let plist = (map (\y -> L.elem y (G.edges g)) (allPairs (map (\x -> namesToVert x m) l)))
>   in case L.notElem True plist of
>       True -> Right (Prob m bsize s g state)
>       False -> Left (CrossError l)

> namesToVert :: String -> VMap -> G.Vertex
> namesToVert name m = case (M.lookup name m) of
>   Just v -> v
>   _      -> error "Name not present in vertex map"

List comprehension partially derived from
https://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell/34045121

> allPairs :: [a] -> [(a, a)]
> allPairs l = [(x, y) | (x:xs) <- tails l, (y:ys) <- tails (reverse l)]