# Tennis

Implementation of the model developped by Agnieszka Madurska for professional single tennis matches prediction.

The full model is described  in the "a.madurska.pdf" file.

parsing.R parse this website: http://www.tennisendirect.net/ , looking for the statistics of the player in the last 50 matches on a relevant surface before the given date.
The "ficheFemme" function, take the name of the female player and the surface("Grass", "Hard" or "Clay") as input.
The output is two csv files:

-The first one contains the date, the name of the opponent, the score of each set, the outcome ("D" for defeat and "V" for victory), the percentage of point won on her serve, percentage of point won on return and surface.

-The second one contains the expect diffrences beetwen the serve-winning probabilities of the two players for a set, given the possible scores of the previous set. Once again, for more details, see a.madurska.pdf.



tennis.R computes the probability of a player winning against another one on a particular surface.
The input of the winmatch function is the name of the first player, the name of the second player and the surface (among "Grass", "Hard" or "Clay").
The output is the probability of the first player winning the match and the number of common opponents of player1 and player2 on the (at most) 50 matches on the same surface.
This function calculates the probabilities using the data from the matches between common opponents of player1 and player2 on the 50 previous matches on a relevant surface. (parsing.R downloaded this data)


ausopen.R, given a WTA tournament, a list of players and the relevant data, computes the probabilities and decide to bet or not and on which player for each match.
It also computes the amount of money earned (or lost) betting on the choosen outcome.



The program gathered the statistics of the 50 previous matches on a relevant surface for every player before the beginning of each tournament.
Then, these statistics were not updated during the tournament.


resultatsAO2013.csv, resultatsRG2013.csv, resultatsWimbledon2013.csv and resultatsUS2013.csv are (in the same ordre) the results of the model on the Australian Open 2013, Roland-Garros 2013, Wimbledon 2013 and the US Open 2013 (tournaments of the WTA Grand Slam).

With no surprise, the more data we have, the higher the accuracy. So, the model seems more accurate on "Hard" and "Clay" surface than on "Grass".
