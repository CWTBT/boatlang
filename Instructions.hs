{-# LANGUAGE GADTSyntax        #-}
import Puzzle
import Problems

import Data.List as L

basic = initProb ["Wolf", "Goat", "Cabbage"] 1 [("Wolf", "Goat"), ("Goat", "Cabbage")]

basic2 = initProb ["Wolf", "Goat", "Rabbit", "Cabbage"] 2 [("Wolf", "Goat"), ("Wolf", "Rabbit"), ("Goat", "Cabbage"), ("Rabbit", "Cabbage")]

ins1 = [["Goat"]
    , []
    , ["Wolf"]
    , ["Goat"]
    , ["Cabbage"]
    , []
    , ["Goat"]]

ins2 = [["Goat", "Rabbit"]
    , []
    , ["Cabbage", "Wolf"]]