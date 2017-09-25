A Scheme program for an iterated prisoner's dilemma game. The procedure play-loop pits two players  against one another for approximately 100 games, then prints out the
average score of each player.
Player strategies are represented as procedures. Each strategy takes two inputs - its own
“history” (that is, a list of all its previous “plays,” where for convenience we will use "c"
to represent cooperate, and "d" to represent defect) and its opponent's “history.” The
strategy returns either the string “c” for “cooperate” or the string “d” for “defect.”