# CLIGame

Very basic text adventure game written in Haskell following [Jack Kelly's suggestions](http://jackkelly.name/blog/archives/2022/05/28/text-mode_games_as_first_haskell_projects/index.html).  
There are still many features yet to be implemented. A list of some I've managed to apply and I found interesting are: 

* Writing and reading from a file, as well as basic String parsing.
  +  Storing and sorting records (./data/records.txt) based on # of kills in each session
  +  Encountering random monsters, configurable by adding a new field in ./data/monsters.txt following the specified format
* Use of Lens library for simplifying work with records
* Use of IO and Maybe monads
* Use of Haddock for generating documentation

* Goals :
  + Fix triple iteration of record string in Game.hs(149:175). Generalize parsing
  + Implementing more suggested features
  + Tidying up the IO puddle: extracting pure functions
  + Command algebraic data type for simplified parsing
  + Integrate databases or JSON files

Suggestions are welcome!

* Project created using cabal: cabal build and cabal run to execute
