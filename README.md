## Advent of Code 2025 in Haskell
Written in haskell because I wanted a change of pace. Some of the solutions are decent, some are pretty bad. The code is handwritten, although I did use AI in place of stack overflow/google most of the time (it is 2025 after all). That said the haskell docs are pretty decent, and were the primary thing I relied on.

### Notes on Specific Solutions
To run any particular day/part just import the relevant module into main and run solvePuzzle on your input.

#### Day 5
Be careful with this one. The actual solvePuzzle is fine, but I did try an alternative version with a hashset that will crash because it exhausts the memory (or at least it did on my machine). In my defense had there been a lot of really small ranges this would have been better than the more sophisticated version.

#### Day 7
Does leverage memoization so it's not unbearably slow. However, I used fgl which was both way overkill and almost certainly caused a performance hit. There are also probably some list operations that are slowing it down as well. Kind of learning how to use the state monad was cool though!

