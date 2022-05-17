{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Lib
  ( generateGame,
    simulateGame,
    countGames,
  )
where

import Control.Monad
import Data.Foldable
import Data.List
import System.IO
import System.Random
import Text.Read

{-
SimulateGame
---------------------------------------------------------------

Description
---------------------------------------------------------------
Simulates a game of Onitama from a file with the a start state
as well as any consequent moves.

Parameters
---------------------------------------------------------------
filePath
  The path to the file containing the game and any additional moves

Returns
---------------------------------------------------------------
IO String
  The game state after the last move
-}
simulateGame :: FilePath -> IO String
simulateGame file = do
  input <- readFile file
  let inputLines = lines input
  if null inputLines
    then return "InvalidFormat"
    else case stringToState (head inputLines) of
      Nothing -> return "InvalidFormat"
      Just state ->
        if isValidState state == "None"
          then do
            case stringMovesToMoves $ stringToMove (tail inputLines) of
              Nothing -> if null (tail inputLines) then return $ stateToString state else return "InvalidFormat"
              Just moves -> return $ simulateGameAux state moves
          else return $ isValidState state

{-
generateGame
---------------------------------------------------------------

Description
---------------------------------------------------------------
Generates a game of Onitama from a seed and a number of moves.

Parameters
---------------------------------------------------------------
Integer
  The seed for the random number generator
Integer
  The number of moves to generate

Returns
---------------------------------------------------------------
IO String
  The game with state and moves
-}
generateGame :: Integer -> Integer -> String
generateGame seed n = do
  let state = GameState (generateCards $ mkStdGen (fromIntegral seed)) [Coordinate 0 2, Coordinate 0 0, Coordinate 0 1, Coordinate 0 3, Coordinate 0 4] [Coordinate 4 2, Coordinate 4 0, Coordinate 4 1, Coordinate 4 3, Coordinate 4 4] 0 False False
  let moves = makeMoves (replicate (fromIntegral n) 1) (mkStdGen (fromIntegral seed)) state []
  stateToString state ++ "\n" ++ movesToString moves

{-
countGames
---------------------------------------------------------------

Description
---------------------------------------------------------------
Counts the number of games from a state. Takes a list of games
finished from the auxiliary function, countGamesAux, and
returns the number of games possible from the state given.

Parameters
---------------------------------------------------------------
Integer
  The number of moves to be counted
FilePath
  The path to the file containing the game state

Returns
---------------------------------------------------------------
IO String
  The number of games with format (total games, player 1 wins, player 2 wins)
-}
countGames :: Integer -> FilePath -> IO String
countGames n file =
  if n < 0
    then return "Number of moves must be greater than or equal to 0"
    else do
      input <- readFile file
      let inputLines = lines input
      if null inputLines
        then return "InvalidFormat"
        else case stringToState (head inputLines) of
          Nothing -> return "InvalidFormat"
          Just state ->
            if isValidState state == "None"
              then do
                let results = countGamesAux (replicate (fromIntegral n) 1) state
                let player1Wins = length $ filter (== 1) results
                let player2Wins = length $ filter (== 2) results
                let games = length results
                return $ show (games, player1Wins, player2Wins)
              else return $ isValidState state

---------------------------------------------------------------------------------------------------------------------
-- Auxiliary functions
---------------------------------------------------------------------------------------------------------------------

{-
countGamesAux
---------------------------------------------------------------

Description
---------------------------------------------------------------
The function used by countGames, which recursively adds a
number to a list each time a game is finished or the maximum
number of moves has been reached.
If player 1 wins, 1 is added to the list.
If player 2 wins, 2 is added to the list.
If a game is finished without a winner, 0 is added to the list.

Parameters
---------------------------------------------------------------
[Int]
  A list used for iterating through the number of moves
GameState
  The game state to be checked

Returns
---------------------------------------------------------------
[Int]
  The list with the number of games finished
-}
countGamesAux :: [Int] -> GameState -> [Int]
countGamesAux _ (GameState _ [] _ _ _ _) = [2]
countGamesAux _ (GameState _ _ [] _ _ _) = [1]
countGamesAux [] _ = [0]
countGamesAux (x : xs) state = do
  validMove <- filter (isValidMove state) (currentPossibleMoves state)
  let newState = movePawn state validMove
  countGamesAux xs newState

-- Takes a list of moves and converts them to string with a newline added after each move.
movesToString :: [GameMoves] -> String
movesToString moves =
  if null moves
    then ""
    else moveToString (head moves) ++ "\n" ++ movesToString (tail moves)

{-
makeMoves
---------------------------------------------------------------

Description
---------------------------------------------------------------
This function is used by generateGame to recursively generate a
list of moves for a game.
The moves are randomly generated, using an index generated by
a random number generator, which has been used with a seed
given by the user.

Parameters
---------------------------------------------------------------
[Int]
  A list used for iterating through the number of moves
StdGen
  The random number generator
GameState
  The game state to make moves from
[GameMoves]
  The list the moves are added to

Returns
---------------------------------------------------------------
[GameMoves]
  The list with the moves added
-}
makeMoves :: [Int] -> StdGen -> GameState -> [GameMoves] -> [GameMoves]
makeMoves [] _ _ moves = moves
makeMoves list seed (GameState _ [] _ _ _ _) moves = moves
makeMoves list seed (GameState _ _ [] _ _ _) moves = moves
makeMoves (x : xs) seed state moves = do
  let index = randomR (0, length (filter (isValidMove state) (currentPossibleMoves state)) - 1) seed
  let move = filter (isValidMove state) (currentPossibleMoves state) !! fst index
  let movedState = movePawn state move
  makeMoves xs (snd index) movedState (moves ++ [move])

-- Generates random cards from the seed
-- HACK: If the random generator somehow shuffles through the same 4 numbers, this function will loop forever
generateCards :: StdGen -> [Card]
generateCards seed = map (allCards !!) (take 5 . nub $ randomRs (0, 15) seed)

{-
currentPossibleMoves
---------------------------------------------------------------

Description
---------------------------------------------------------------
This function is used by multiple functions to get a list of
possible moves from a game state.
The function checks for both super moves as well as normal moves.

Parameters
---------------------------------------------------------------
GameState
  The game state to get the possible moves from. This state
  includes the pawns and the cards in the game.

Returns
---------------------------------------------------------------
[GameMoves]
  The list of possible moves from the given state
-}
currentPossibleMoves :: GameState -> [GameMoves]
currentPossibleMoves (GameState cards@[card1, card2, _, _, _] player1Coordinates _ 0 _ _) = do
  card <- [card1, card2]
  coords <- player1Coordinates
  super <- [True, False]
  moves <- if super then superCard else possibleMoves card
  return (GameMoves card coords (Coordinate (x coords + x moves) (y coords + y moves)) super)
currentPossibleMoves (GameState cards@[_, _, card3, card4, _] _ player2Coordinates 1 _ _) = do
  card <- [card3, card4]
  coords <- player2Coordinates
  super <- [True, False]
  moves <- if super then superCard else possibleMoves card
  return (GameMoves card coords (Coordinate (x coords - x moves) (y coords - y moves)) super)

-- | Returns a list of possible moves from a list of possibly GameMoves
stringMovesToMoves :: [Maybe GameMoves] -> Maybe [GameMoves]
stringMovesToMoves [] = Just []
stringMovesToMoves (Nothing : _) = Nothing
stringMovesToMoves (Just move : rest) = case stringMovesToMoves rest of
  Just restMoves -> Just (move : restMoves)

{-
simulateGameAux
---------------------------------------------------------------

Description
---------------------------------------------------------------
This function is used by simulateGame to recursively simulate
a game using the game state and moves given.
For each move in the list, the function checks if the move is
valid, and if it is, it makes the move.
Pattern matching is used to check if part of the state is valid,
namely if the game is over but not reported as such.

Parameters
---------------------------------------------------------------
GameState
  The game state to simulate from
[GameMoves]
  The list of moves to simulate

Returns
---------------------------------------------------------------
String
  The result of the simulation. Beginning with the game state,
  followed by the moves.
 -}
simulateGameAux :: GameState -> [GameMoves] -> String
simulateGameAux state [] = stateToString state
simulateGameAux state@(GameState _ [] _ _ _ _) (move : xs) = "InvalidMove " ++ moveToString move
simulateGameAux state@(GameState _ _ [] _ _ _) (move : xs) = "InvalidMove " ++ moveToString move
simulateGameAux state (move : ms) =
  if isValidMove state move
    then simulateGameAux (movePawn state move) ms
    else "InvalidMove " ++ moveToString move

-- Sort the first two cards and the next two cards in ascending order
-- Format: ""cardName", "cardName", ...
sortCardNames :: [String] -> String
sortCardNames [card1, card2, card3, card4, card5] =
  show (sort [card1, card2] ++ sort [card3, card4] ++ [card5])

-- Sort the first two cards and the next two cards in ascending order
sortCardNamesToList :: [String] -> [String]
sortCardNamesToList [card1, card2, card3, card4, card5] =
  sort [card1, card2] ++ sort [card3, card4] ++ [card5]

-- Sort the coordinates of the pawns in ascending order
sortPawnCoords :: [(Int, Int)] -> [(Int, Int)]
sortPawnCoords (x : xs) = x : sort xs

-- Convert a list of coordinates to a tuple of coordinates
pawnCoordsToTuple :: [Coordinate] -> [(Int, Int)]
pawnCoordsToTuple [] = []
pawnCoordsToTuple xs = sortPawnCoords $ map (\(Coordinate x y) -> (x, y)) xs

-- Convert a game state to a string representation.
stateToString :: GameState -> String
stateToString (GameState cards player1PawnCoordinates player2PawnCoordinates turn player1Super player2Super) =
  "(" ++ sortCardNames (map cardName cards) ++ ","
    ++ show (pawnCoordsToTuple player1PawnCoordinates)
    ++ ","
    ++ show (pawnCoordsToTuple player2PawnCoordinates)
    ++ ","
    ++ show turn
    ++ ","
    ++ show player1Super
    ++ ","
    ++ show player2Super
    ++ ")"

-- Formats move to string
-- Format: ((x1,y1),(x2,y2), "cardName")
-- Super format: ((x1,y1),(x2,y2), "Super cardName")
moveToString :: GameMoves -> String
moveToString (GameMoves (Card cardName _) (Coordinate x1 y1) (Coordinate x2 y2) isSuperMove) =
  "((" ++ show x1 ++ "," ++ show y1 ++ "),(" ++ show x2 ++ "," ++ show y2 ++ "),\"" ++ (if isSuperMove then "Super " else "") ++ cardName ++ "\")"

-- Formats a string state to the internal state representation
stringToState :: String -> Maybe GameState
stringToState state = do
  case stringToStateTuple state of
    Just state -> return (stringToStateAux state)
    Nothing -> Nothing

-- Tries to read a string as an internal representation of the game state
stringToStateTuple :: String -> Maybe ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool)
stringToStateTuple string = readMaybe string :: Maybe ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool)

-- Parses tuple to GameState
-- String to card
-- [(Int, Int)] to Coordinate
stringToStateAux :: ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool) -> GameState
stringToStateAux (cards, player1PawnCoordinates, player2PawnCoordinates, turn, player1Super, player2Super) =
  GameState (stringToCardList cards) (map pairToCoordinate player1PawnCoordinates) (map pairToCoordinate player2PawnCoordinates) turn player1Super player2Super

-- Finds each card from a list of strings
stringToCardList :: [String] -> [Card]
stringToCardList [] = []
stringToCardList (cardName : xs) =
  case findCard cardName of
    Just card -> card : stringToCardList xs
    Nothing -> []

-- Converts a pair of coordinates to a coordinate data type
pairToCoordinate :: (Int, Int) -> Coordinate
pairToCoordinate (x, y) = Coordinate x y

{-
isValidState
---------------------------------------------------------------

Description
---------------------------------------------------------------
This function is used to check if a game state is valid. It
uses guards to check whether or not the game state parameter
is valid, and if not it returns the correct error message.

Parameters
---------------------------------------------------------------
GameState
  The game state to check

Returns
---------------------------------------------------------------
String
  The error message if the game state is invalid, otherwise
  "None" indicating that no error occurred.
 -}
isValidState :: GameState -> String
isValidState (GameState cards player1Coords player2Coords turn player1Super player2Super)
  | null player1Coords && null player2Coords = "InvalidState" -- no pawns
  | length (nub (player1Coords ++ player2Coords)) > 10 = "InvalidState" -- too many pawns
  | length cards /= 5 = "InvalidState" -- not enough cards (or too many)
  | turn < 0 || turn > 1 = "InvalidFormat" -- turn is not 0 or 1
  | not $ all isValidPosition (player1Coords ++ player2Coords) = "InvalidState" -- invalid position
  | length (nub (player1Coords ++ player2Coords)) /= length (player1Coords ++ player2Coords) = "InvalidState" -- duplicate position
  | not (null player1Coords) && sortCoordinates (tail player1Coords) /= tail player1Coords = "InvalidFormat" -- player1 pawns are not in order
  | not (null player2Coords) && sortCoordinates (tail player2Coords) /= tail player2Coords = "InvalidFormat" -- player2 pawns are not in order
  | sortCardNamesToList (map cardName cards) /= map cardName cards = "InvalidFormat" -- cards are not in order
  | not (null player1Coords) && head player1Coords == Coordinate 4 2 && not (null player2Coords) = "InvalidState" -- player1 has won
  | not (null player2Coords) && head player2Coords == Coordinate 0 2 && not (null player1Coords) = "InvalidState" -- player2 has won
  | otherwise = "None" -- valid state

-- Sorts coordinates in ascending order
sortCoordinates :: [Coordinate] -> [Coordinate]
sortCoordinates = sortOn (\(Coordinate x y) -> (x, y))

-- Converts strings to a list of GameMoves if possible, otherwise returns Nothing in list
stringToMove :: [String] -> [Maybe GameMoves]
stringToMove [] = return Nothing
stringToMove move = do
  string <- move
  let readString = readMaybe string :: Maybe ((Int, Int), (Int, Int), String)
  case readString of
    Just move -> return (Just (stringToMoveAux move))
    Nothing -> return Nothing

-- Converts from tuple to GameMoves data type
stringToMoveAux :: ((Int, Int), (Int, Int), String) -> GameMoves
stringToMoveAux ((x1, y1), (x2, y2), cardName) =
  if isPrefixOf "Super " cardName
    then case findCard (drop 6 cardName) of -- drop "Super " (with space)
      Just card -> GameMoves card (Coordinate x1 y1) (Coordinate x2 y2) True
      Nothing -> GameMoves (Card (drop 6 cardName) []) (Coordinate x1 y1) (Coordinate x2 y2) True
    else case findCard cardName of
      Just card -> GameMoves card (Coordinate x1 y1) (Coordinate x2 y2) False
      Nothing -> GameMoves (Card cardName []) (Coordinate x1 y1) (Coordinate x2 y2) False

-- Checks whether or not the position is valid on the board
isValidPosition :: Coordinate -> Bool
isValidPosition (Coordinate x y) = x >= 0 && x <= 4 && y >= 0 && y <= 4

-- CHecks whether or not the card is valid based on various checks
isValidCard :: Int -> Coordinate -> Coordinate -> Card -> [Card] -> Bool
isValidCard 0 (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  not (null (possibleMoves card)) && elem card allCards && elem (Coordinate (x2 - x1) (y2 - y1)) (possibleMoves card)
isValidCard 1 (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  not (null (possibleMoves card)) && elem card allCards && elem (Coordinate (x1 - x2) (y1 - y2)) (possibleMoves card)

-- Checks whether or not a super card is valid based on various checks
isValidSuperCard :: Coordinate -> Coordinate -> Card -> [Card] -> Bool
isValidSuperCard (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  not (null (possibleMoves card)) && elem card allCards && elem (Coordinate (x2 - x1) (y2 - y1)) superCard

-- Checks whether or not a move is valid, based on the game state and the move
isValidMove state@(GameState cards pawnCoordinates opponentPawns 0 _ _) moves@(GameMoves card source destination False) =
  isValidPosition destination
    && elem source pawnCoordinates
    && notElem destination pawnCoordinates
    && isValidCard 0 source destination card (take 2 cards)
isValidMove (GameState [_, _, card3, card4, _] opponentPawns pawnCoordinates 1 _ _) (GameMoves card source destination False) =
  isValidPosition destination
    && elem source pawnCoordinates
    && notElem destination pawnCoordinates
    && isValidCard 1 source destination card [card3, card4]
isValidMove (GameState cards pawnCoordinates opponentPawns 0 False _) (GameMoves card source destination True) =
  isValidPosition destination
    && elem source pawnCoordinates
    && notElem destination pawnCoordinates
    && isValidSuperCard source destination card (take 2 cards)
isValidMove (GameState [_, _, card3, card4, _] opponentPawns pawnCoordinates 1 _ False) (GameMoves card source destination True) =
  isValidPosition destination
    && elem source pawnCoordinates
    && notElem destination pawnCoordinates
    && isValidSuperCard source destination card [card3, card4]
isValidMove _ _ = False

-- Checks whether or not a state combined with a move results in a win for either party
hasWon :: GameState -> GameMoves -> Bool
hasWon (GameState _ player1Coords player2Coords 0 _ _) (GameMoves _ source destination _) =
  head player1Coords == source && destination == Coordinate 4 2 || (head player2Coords == destination)
hasWon (GameState _ player1Coords player2Coords 1 _ _) (GameMoves _ source destination _) =
  head player2Coords == source && destination == Coordinate 0 2 || (head player1Coords == destination)

-- Moves the pawn, changes the state accordingly, and checks whether or not the game has been won by this move.
movePawn :: GameState -> GameMoves -> GameState
movePawn state@(GameState [card1, card2, card3, card4, card5] player1Coords player2Coords 0 superMove1 superMove2) moves@(GameMoves card source destination isSuper) =
  GameState
    ( if card == card1
        then [card5, card2, card3, card4, card1]
        else [card1, card5, card3, card4, card2]
    )
    ( takeWhile (/= source) player1Coords
        ++ [destination]
        ++ drop 1 (dropWhile (/= source) player1Coords)
    )
    ( if hasWon state moves
        then []
        else filter (/= destination) player2Coords
    )
    1
    (superMove1 || isSuper)
    superMove2
movePawn state@(GameState [card1, card2, card3, card4, card5] player1Coords player2Coords 1 superMove1 superMove2) moves@(GameMoves card source destination isSuper) =
  GameState
    ( if card == card3
        then [card1, card2, card5, card4, card3]
        else [card1, card2, card3, card5, card4]
    )
    ( if hasWon state moves
        then []
        else filter (/= destination) player1Coords
    )
    ( takeWhile (/= source) player2Coords
        ++ [destination]
        ++ drop 1 (dropWhile (/= source) player2Coords)
    )
    0
    superMove1
    (superMove2 || isSuper)

-- A list of all of the cards in the game
allCards :: [Card]
allCards =
  [ Card "Boar" [Coordinate 0 (-1), Coordinate 0 1, Coordinate 1 0],
    Card "Cobra" [Coordinate 0 (-1), Coordinate 1 1, Coordinate (-1) 1],
    Card "Crab" [Coordinate 1 0, Coordinate 0 2, Coordinate 0 (-2)],
    Card "Crane" [Coordinate 1 0, Coordinate (-1) (-1), Coordinate (-1) 1],
    Card "Dragon" [Coordinate 2 (-2), Coordinate 2 2, Coordinate (-1) (-1), Coordinate (-1) 1],
    Card "Eel" [Coordinate 1 (-1), Coordinate 0 1, Coordinate (-1) (-1)],
    Card "Elephant" [Coordinate 0 (-1), Coordinate 1 (-1), Coordinate 1 1, Coordinate 0 1],
    Card "Frog" [Coordinate 0 (-2), Coordinate 1 (-1), Coordinate (-1) 1],
    Card "Goose" [Coordinate (-1) 1, Coordinate 0 (-1), Coordinate 0 1, Coordinate 1 (-1)],
    Card "Horse" [Coordinate 0 (-1), Coordinate 1 0, Coordinate (-1) 0],
    Card "Mantis" [Coordinate 1 (-1), Coordinate 1 1, Coordinate (-1) 0],
    Card "Ox" [Coordinate 1 0, Coordinate 0 1, Coordinate (-1) 0],
    Card "Rabbit" [Coordinate (-1) (-1), Coordinate 1 1, Coordinate 0 2],
    Card "Rooster" [Coordinate 0 (-1), Coordinate (-1) (-1), Coordinate 1 1, Coordinate 0 1],
    Card "Tiger" [Coordinate 2 0, Coordinate (-1) 0],
    Card "Monkey" [Coordinate 1 1, Coordinate 1 (-1), Coordinate (-1) 1, Coordinate (-1) (-1)]
  ]

-- A list of all of the possible moves for a super card.
superCard :: [Coordinate]
superCard =
  [ Coordinate 0 1,
    Coordinate 1 0,
    Coordinate 1 1,
    Coordinate (-1) 0,
    Coordinate (-1) 1,
    Coordinate 0 (-1),
    Coordinate 1 (-1),
    Coordinate (-1) (-1)
  ]

-- A function to find a card from the card name
findCard :: String -> Maybe Card
findCard name = find (\card -> cardName card == name) allCards

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- Data type used for coordinates
data Coordinate = Coordinate
  { x :: Int,
    y :: Int
  }
  deriving (Eq)

-- Data type used for cards
data Card = Card
  { cardName :: String,
    -- | Coordinate from the pawn, i.e. not the actual position on the board
    possibleMoves :: [Coordinate]
  }
  deriving (Eq)

-- Data type for the gamestate
data GameState
  = GameState
      [Card]
      -- ^ Cards in the deck
      [Coordinate]
      -- ^ Coordinates of the player 1's pawns
      [Coordinate]
      -- ^ Coordinates of the player 2's pawns
      Int
      -- ^ 0 for player 1, 1 for player 2
      Bool
      -- ^ Whether or not the player1 has used super card
      Bool
      -- ^ Whether or not the player2 has used super card

-- Data type for the game moves
data GameMoves
  = GameMoves
      Card
      Coordinate
      Coordinate
      Bool
