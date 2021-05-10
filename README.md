The goal of the project was to devise a simple language that could describe 
arbitrarily-sized [river-crossing problems](https://en.wikipedia.org/wiki/River_crossing_puzzle) and their solutions.

There are several variants of these types of problems, but the specific
kind this language covers are of the [Wolf, Goat, and Cabbage](https://en.wikipedia.org/wiki/Wolf,_goat_and_cabbage_problem)
type , wherein one must move all items from one side
of a river to the other, with the constraint that some items
may not be left alone with each other.

I thought this would be an interesting subject to make a language around
because the solutions to these problems can be thought of in terms of
graph theory; if we imagine each "item" as a vertex, and edges between
each vertex representing that the two attached vertices may not be left
alone on the same side of the river, then a valid "move" is one which
creates a vertex cover from this graph.

Haskell already has some nice libraries for working with graphs,
so I chose to build this language as an EDSL. As a result, most of the
work I did was abstracting the puzzle into relevent data structures,
pretty-printing the results, and devising ways to validate a solution.
The "validation" process very closely resembles the error handling we
did in languages like Arith.

Currently, the language can validate any given solution for a problem.
My hopes were to also implement a validation process for the problems
themselves, such that one cannot create a problem that is impossible
to solve. To my understanding, the problem boils down to whether it
is possible to create a vertex cover of size *k* from an arbitrary graph.
I'm reasonable sure this is possible in < polynomial time, but my Haskell 
know-how was a bit too weak to implement the algorithm with the time I had.

## Files
Most of the backend is handled/explained in Problems.lhs. Important types are:

1. **Prob**: Encapsulates all information necessary to represent a given state of the puzzle.
2. **ProbError**: Represents the various ways that a given solution to a problem could be invalid.
3. **VMap**: Simple type synonym for a map from strings to ints. Haskell graphs use ints to refer to vertices, so we have to maintain some mapping between them for pretty-printing purposes.
4. **State**: Represents the two sides of the river, and what items are currently on each side.

Puzzle.lhs is where most of the validation for solutions happens, as well as where most of the
interesting work on the graphs happens.

Instructions.hs is an example file where you can define new problems and enter solutions. There are some examples
already there, but feel free to add more/play around with it.