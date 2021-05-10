> {-# LANGUAGE GADTSyntax        #-}
> module Problems where
> import qualified Data.Graph as G
> import qualified Data.Map   as M
> import qualified Data.List  as L

> type VMap = M.Map String G.Vertex
> type State = ([String], [String])

> data Side where
>   L :: Side
>   R :: Side
>   deriving (Show, Eq)

VMap: Vertices in the graph are represented
with integers, this maps string names to
their corresponding integer representations

Int: How many items the boat can carry at once

Side: What "side of the river" the boat is
currently on, determines how crossing will
change the state

G.Graph: Represents all conflicts between
items. Each vertex represents an item. Any
two vertices that share an edge must not be
left on the same side of the river

State: Two lists, the first of which contains
all items on the "left side", the other
all items on the "right side"

> data Prob where
>   Prob :: VMap -> Int -> Side -> G.Graph -> State -> Prob
>   deriving (Show)

> data ProbError where
>   CrossError :: [String]  -> ProbError
>   LoadError :: Int -> Int -> ProbError
>   NoneError :: String   -> ProbError
>   AbsentError :: String -> ProbError

> showProb :: Prob -> String
> showProb (Prob m bsize s g (lside, rside)) = case s of 
>   R -> (showState lside "[") ++ " |-->| " ++ (showState rside "[")
>   L -> (showState lside "[") ++ " |<--| " ++ (showState rside "[")

> showErr :: ProbError -> String
> showErr e = case e of
>   CrossError state      -> "INVALID CROSS: [" ++ (showState state "") ++ " is not valid under the problem rules"
>   LoadError lsize bsize -> "INVALID CROSS: You tried to move " ++ (show lsize) ++ " things, but the boat can only carry " ++ (show bsize)
>   NoneError load        -> "INVALID CROSS: You tried to move a " ++ load ++ ", which does not exist in this problem"
>   AbsentError load      -> "INVALID CROSS: You tried to cross with a " ++ load ++ " when it wasn't on your side of the river"

> showState :: [String] -> String -> String
> showState [] s = s ++ "]"
> showState (l:list) s = case list of
>   [] -> showState list (s ++ l)
>   _ -> showState list (s ++ l ++ ", ")

fillMap populates the VMap with the names
of their items and their integer representations.

> fillMap :: [String] -> VMap -> VMap
> fillMap [] m         = m
> fillMap (c : conf) m = case M.lookup c m of
>   Just _  -> fillMap conf m
>   Nothing -> fillMap conf (M.insert c (M.size m) m)

When making a problem, the user supplies a list
of pairs of strings, representing conflicts
between items. The following functions convert
from [(String, String)] to [(G.Vertex, G.Vertex)],
a list of edges that can be used to construct the
graph.

> mapVs :: [(String, String)] -> [(G.Vertex, G.Vertex)] -> VMap -> [(G.Vertex, G.Vertex)]
> mapVs [] vs m        = vs
> mapVs (c:confs) vs m = mapVs confs ((mapV c m):vs) m  

> mapV :: (String, String) -> VMap -> (G.Vertex, G.Vertex)
> mapV (p1, p2) m = case (M.lookup p1 m, M.lookup p2 m) of
>   (Just x, Just y) -> (x, y)
>   _                -> error "At least one relationship contains items that were not listed"

> initProb :: [String] -> Int -> [(String, String)] -> Prob
> initProb names bsize confs = let m = fillMap (L.sort names) M.empty
>   in Prob m bsize R (G.buildG (0, length confs) (mapVs confs [] m)) ([], names)


Remnants of misbegotten attempts to validate
a problem by demonstrating a vertex cover of
a certain size could be made.

 findVertexCover :: Integer -> [G.Edge] -> [G.Vertex] -> [G.Vertex]
 findVertexCover msize ((e1, e2):rest) cov 
   | (length ((e1, e2):rest)) == 0 = cov
   | msize == 0                    = [-1]
   | otherwise                     = findVertexCover (msize-1) (filter (\(x, y) -> x /= e1 &&  y /= e1) rest) (e1:cov)