# Onitama

## Representation
The three main representations are the following: 
* Game State
* Game Moves
* Card
The way I have chosen to represent these in the code is a lot like how they are represented in the string format but directly converted to code.
### Game state
The game state contains the following: 
* A list of cards (using the Cards type).
* two lists of coordinates, one list for each player. 
* An integer representing whether it is player 1 or 2 (0 or 1). 
* Two booleans representing whether or not the respective players have used their super moves.

### Game Moves
The game moves contain the following:
* A card.
* A coordinate for the source of the card.
* A coordinate for the destination of the card.
* A bool to represent whether or not it is a super card.

### Card
The card type contains the following: 
* A string representing the name of the card
* A list of coordinates representing where the card allows the pawn to move. 
The coordinates in the card type are the coordinates relative to the pawn, meaning I later calculate where the pawn should move based on the card.

Additionally, I have created another type for the coordinates, which only hold two Ints, one representing the y-axis and one representing the x-axis, as I believe it makes the code much more readable.

The way I store the cards (to later fetch them) is by using a function which contains a list of all the cards. 

## Challenges
I had quite a few challenges towards the end of the project. These challenges were mostly in the functions `countGames` and `generateGame`. 
For a reason that I later found out to be trivial, my `countGames` function did not generate correct correct games according to my
`simulateGame` function. The reason, I found out, was that one of my cards in the function with a list of cards i mentioned earlier,
was incorrect. This was very odd to me, because all of my checks from previous submissions worked without issue. Other than this no
major issue had occured.


## Expression coverage not reaching 100%
Due to my usage of number literals in the code, my expression coverage does not reach 100%. However, this is the only required coverage that I fail.
