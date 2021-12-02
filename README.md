# Advent of Code 2021
This repo contains my code for AoC 2021

My goal is to do this in Haskell most of the days, 
    but if a lack of time or skill leads me astray from the path of purity I may also fall back to Julia or 
    (if all else fails) Python.

Running stuff:
## Haskell
- Install [Stack](https://docs.haskellstack.org/en/stable/README/)
- Go to the `HaskellCode/aoc/` folder and run `stack build`
- Open ghci using `stack ghci`, this will open an interactive session with everything loaded
- Call whatever function you want to test, i.e. `fib` 
  (WARNING: it's not recommended running `fib` in an IntelliJ terminal unless you like stuff breaking)

## Julia
- Install [Julia](https://julialang.org/) (duh)

### Running main (will show all implemented answers)
- Go to `JuliaCode/` in a terminal and run `julia src/main.jl`

### Running from repl
- Go to `JuliaCode/` in a terminal and run `julia`
- Press `]` to go into the `pkg>` thingymabop
- type `activate .`
- Press escape to get out of the `pkg>` thingymabop
- `using JuliaCode`
- Run whatever you want (as long as it's exported), i.e. `day1q1()`

## Prolog
- Install [SWI-Prolog](https://www.swi-prolog.org/)
- Go to `PrologCode/` in a terminal and run `swipl day1.pl`
- Type `X.` and have the big funny
- Ask Prolog a question like `computeDay1Q2(Count).`, which means "What value of Count is the answer to Day 1 Question 2"
- Prolog will grant you the answer (though it seems unsure whether that's the only answer)