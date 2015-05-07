# chess_problem #

A sample Scala project counting the number of possible placements of a number of chess pieces (pawns excluded) on a chessboard of given dimensions.

## Requirements ##
sbt in version not lesser than 0.13 (available [here][sbt]). Others may work, but that's not been tested.


## Execution ##
Calling `sbt run` in the root directory of the project causes all unresolved dependencies to be fetched, compiles the sources and runs the program.

Upon start, the program will ask for arguments by printing requests to standard output (e.g. `Number of columns:`).
The appropriate argument is to be input to standard input followed by a newline character.

After calculations are done, the program will return the number of possible placements to standard output.

[sbt]: http://www.scala-sbt.org/download.html   "Download sbt"