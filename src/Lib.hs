{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use infix" #-}

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

-- | simulateGame
-- | The function reads input from a file, does some magic stuff and then simulates a game
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
            case stringMovesToMoves [] $ stringToMove (tail inputLines) of
              Nothing -> return "InvalidFormat"
              Just moves -> return $ simulateGameAux state moves
          else return $ isValidState state

generateGame :: Integer -> Integer -> String
generateGame _ _ = undefined

countGames :: Integer -> FilePath -> IO String
countGames _ _ = undefined

-- | Get moves from a list of strings and return a list of moves
stringMovesToMoves :: [GameMoves] -> [Maybe GameMoves] -> Maybe [GameMoves]
stringMovesToMoves moves [] = Just []
stringMovesToMoves moves (Nothing : _) = Nothing
stringMovesToMoves moves (Just move : rest) = case stringMovesToMoves moves rest of
  Nothing -> Nothing
  Just restMoves -> Just (move : restMoves)

-- | Auxiliary method for simulateGame
simulateGameAux :: GameState -> [GameMoves] -> String
simulateGameAux state [] = stateToString state
simulateGameAux state@(GameState _ [] _ _ _ _) (move : xs) = "InvalidMove " ++ moveToString move
simulateGameAux state@(GameState _ _ [] _ _ _) (move : xs) = "InvalidMove " ++ moveToString move
simulateGameAux state (move : ms) =
  if isValidMove state move
    then simulateGameAux (movePawn state move) ms
    else "InvalidMove " ++ moveToString move

-- | Sort the first two cards and the next two cards in ascending order
-- | Format: ""cardName", "cardName", ...
sortCardNames :: [String] -> String
sortCardNames [card1, card2, card3, card4, card5] =
  show (sort [card1, card2] ++ sort [card3, card4] ++ [card5])
sortCardNames _ = ""

sortCardNamesToList :: [String] -> [String]
sortCardNamesToList [card1, card2, card3, card4, card5] =
  sort [card1, card2] ++ sort [card3, card4] ++ [card5]
sortCardNamesToList _ = []

--------------------------------------------------------------------------------

-- | to string methods

--------------------------------------------------------------------------------

sortPawnCoords :: [(Int, Int)] -> [(Int, Int)]
sortPawnCoords [] = []
sortPawnCoords (x : xs) = x : sort xs

pawnCoordsToTuple :: [Coordinate] -> [(Int, Int)]
pawnCoordsToTuple [] = []
pawnCoordsToTuple xs = sortPawnCoords $ map (\(Coordinate x y) -> (x, y)) xs

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

-- | Formats move to string
-- | Format: ((x1,y1),(x2,y2), "cardName")
-- | Super format: ((x1,y1),(x2,y2), "Super cardName")
moveToString :: GameMoves -> String
moveToString (GameMoves (Card cardName _) (Coordinate x1 y1) (Coordinate x2 y2) isSuperMove) =
  "((" ++ show x1 ++ "," ++ show y1 ++ "),(" ++ show x2 ++ "," ++ show y2 ++ "),\"" ++ (if isSuperMove then "Super " else "") ++ cardName ++ "\")"

-----------------------------------------------------------------------------------------------------------------------

-- | string to state

-----------------------------------------------------------------------------------------------------------------------

stringToState :: String -> Maybe GameState
stringToState state = do
  case stringToStateTuple state of
    Just state -> return (stringToStateAux state)
    Nothing -> Nothing

stringToStateTuple :: String -> Maybe ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool)
stringToStateTuple string = readMaybe string :: Maybe ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool)

-- | Parses tuple to GameState
-- | String to card
-- | [(Int, Int)] to Coordinate
stringToStateAux :: ([String], [(Int, Int)], [(Int, Int)], Int, Bool, Bool) -> GameState
stringToStateAux (cards, player1PawnCoordinates, player2PawnCoordinates, turn, player1Super, player2Super) =
  GameState (stringToCardList cards) (map pairToCoordinate player1PawnCoordinates) (map pairToCoordinate player2PawnCoordinates) turn player1Super player2Super

stringToCardList :: [String] -> [Card]
stringToCardList [] = []
stringToCardList (cardName : xs) =
  case findCard cardName of
    Just card -> card : stringToCardList xs
    Nothing -> []

pairToCoordinate :: (Int, Int) -> Coordinate
pairToCoordinate (x, y) = Coordinate x y

-----------------------------------------------------------------------------------------------------------------------

-- | see if state is valid

-----------------------------------------------------------------------------------------------------------------------

isValidState :: GameState -> String
isValidState (GameState cards player1Coords player2Coords turn player1Super player2Super)
  | null player1Coords && null player2Coords = "InvalidState" -- no pawns
  | length (removeDuplicates (player1Coords ++ player2Coords)) > 10 = "InvalidState" -- too many pawns
  | length cards /= 5 = "InvalidState" -- not enough cards (or too many)
  | turn < 0 || turn > 1 = "InvalidFormat" -- turn is not 0 or 1
  | not $ all isValidPosition (player1Coords ++ player2Coords) = "InvalidState" -- invalid position
  | length (removeDuplicates (player1Coords ++ player2Coords)) /= length (player1Coords ++ player2Coords) = "InvalidState" -- duplicate position
  | sortCoordinates (tail player1Coords) /= tail player1Coords = "InvalidFormat" -- player1 pawns are not in order
  | sortCoordinates (tail player2Coords) /= tail player2Coords = "InvalidFormat" -- player2 pawns are not in order
  | sortCardNamesToList (map cardName cards) /= map cardName cards = "InvalidFormat" -- cards are not in order
  | player1Coords == player2Coords = "InvalidState" -- pawns are the same
  | head player1Coords == Coordinate 4 2 && not (null player2Coords) = "InvalidState" -- player1 has won
  | head player2Coords == Coordinate 0 2 && not (null player1Coords) = "InvalidState" -- player2 has won
  | otherwise = "None" -- valid state

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

sortCoordinates :: [Coordinate] -> [Coordinate]
sortCoordinates = sortOn (\(Coordinate x y) -> (x, y))

-----------------------------------------------------------------------------------------------------------------------

-- | Convert string to move

-----------------------------------------------------------------------------------------------------------------------

-- FIX: For some reason stringToMoveTuple would not work, this is a (probably) temporary fix.

stringToMove :: [String] -> [Maybe GameMoves]
stringToMove [] = return Nothing
stringToMove move = do
  string <- move
  let readString = readMaybe string :: Maybe ((Int, Int), (Int, Int), String)
  case readString of
    Just move -> return (Just (stringToMoveAux move))
    Nothing -> return Nothing

stringToMoveAux :: ((Int, Int), (Int, Int), String) -> GameMoves
stringToMoveAux ((x1, y1), (x2, y2), cardName) =
  if isPrefixOf "Super " cardName
    then case findCard (drop 6 cardName) of -- drop "Super " (with space)
      Just card -> GameMoves card (Coordinate x1 y1) (Coordinate x2 y2) True
      Nothing -> GameMoves (Card cardName []) (Coordinate x1 y1) (Coordinate x2 y2) True
    else case findCard cardName of
      Just card -> GameMoves card (Coordinate x1 y1) (Coordinate x2 y2) False
      Nothing -> GameMoves (Card cardName []) (Coordinate x1 y1) (Coordinate x2 y2) False

-----------------------------------------------------------------------------------------------------------------------

-- | validation checks for cards

-----------------------------------------------------------------------------------------------------------------------

isValidPosition :: Coordinate -> Bool
isValidPosition (Coordinate x y) = x >= 0 && x <= 4 && y >= 0 && y <= 4

isValidCard :: Int -> Coordinate -> Coordinate -> Card -> [Card] -> Bool
isValidCard 0 (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  elem card allCards && elem (Coordinate (x2 - x1) (y2 - y1)) (possibleMoves card)
isValidCard 1 (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  elem card allCards && elem (Coordinate ((x2 - x1) * (-1)) ((y2 - y1) * (-1))) (possibleMoves card)
isValidCard _ _ _ _ _ = error "Invalid turn"

isValidSuperCard :: Coordinate -> Coordinate -> Card -> [Card] -> Bool
isValidSuperCard (Coordinate x1 y1) (Coordinate x2 y2) card allCards =
  elem card allCards && elem (Coordinate (x2 - x1) (y2 - y1)) (possibleMoves superCard)

-- | Checks whether or not a coordinate is a valid position on the board
-- | Factors:
-- | - Source and destination coordinates are valid
-- | - Destination is not one of your own pawns
-- | - You're trying to move a pawn that is yours
isValidMove :: GameState -> GameMoves -> Bool
isValidMove state@(GameState cards pawnCoordinates opponentPawns 0 _ _) moves@(GameMoves card source destination False) =
  isValidPosition source && isValidPosition destination && elem source pawnCoordinates && notElem destination pawnCoordinates && isValidCard 0 source destination card (take 2 cards)
isValidMove (GameState cards opponentPawns pawnCoordinates 1 _ _) (GameMoves card source destination False) =
  isValidPosition source && isValidPosition destination && elem source pawnCoordinates && notElem destination pawnCoordinates && isValidCard 1 source destination card (take 2 (drop 2 cards))
isValidMove (GameState cards pawnCoordinates opponentPawns  0 False _) (GameMoves card source destination True) =
  isValidPosition source && isValidPosition destination && elem source pawnCoordinates && notElem destination pawnCoordinates && isValidSuperCard source destination card (take 2 cards)
isValidMove (GameState cards opponentPawns pawnCoordinates 1 _ False) (GameMoves card source destination True) =
  isValidPosition source && isValidPosition destination && elem source pawnCoordinates && notElem destination pawnCoordinates && isValidSuperCard source destination card (take 2 (drop 2 cards))
isValidMove _ _ = False

hasWon :: GameState -> GameMoves -> Bool
hasWon (GameState _ player1Coords player2Coords 0 _ _) (GameMoves _ source destination _) =
  head player1Coords == source && x destination == 4 && y destination == 2 || (head player1Coords == destination)
hasWon (GameState _ player1Coords player2Coords 1 _ _) (GameMoves _ source destination _) =
  head player2Coords == source && x destination == 0 && y destination == 2 || (head player2Coords == destination)
hasWon _ _ = False

-- | Along with moving the pawn, it also checks whether or not the game has been won
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
movePawn _ _ = error "Invalid move"

-- | NOTE: x and y are flipped because of the way the board is drawn
allCards :: [Card]
allCards =
  [ Card "Tiger" [Coordinate (-1) 0, Coordinate 2 0],
    Card "Dragon" [Coordinate (-1) (-1), Coordinate (-1) 1, Coordinate 1 (-2), Coordinate 2 1],
    Card "Frog" [Coordinate 0 (-2), Coordinate (-1) 1, Coordinate 1 (-1)],
    Card "Rabbit" [Coordinate (-1) (-1), Coordinate 1 1, Coordinate 0 2],
    Card "Crab" [Coordinate 0 (-2), Coordinate 0 2, Coordinate 1 0],
    Card "Elephant" [Coordinate 0 (-1), Coordinate 1 (-1), Coordinate 1 1, Coordinate 1 0],
    Card "Goose" [Coordinate 1 (-1), Coordinate 0 (-1), Coordinate 0 1, Coordinate 1 (-1)],
    Card "Rooster" [Coordinate 0 (-1), Coordinate (-1) (-1), Coordinate 0 1, Coordinate 1 1],
    Card "Monkey" [Coordinate 1 (-1), Coordinate (-1) (-1), Coordinate 1 1, Coordinate 1 (-1)],
    Card "Mantis" [Coordinate (-1) 0, Coordinate 1 (-1), Coordinate 1 1],
    Card "Horse" [Coordinate 0 (-1), Coordinate 1 0, Coordinate (-1) 0],
    Card "Ox" [Coordinate 1 0, Coordinate (-1) 0, Coordinate 0 1],
    Card "Crane" [Coordinate (-1) (-1), Coordinate (-1) 1, Coordinate 1 0],
    Card "Boar" [Coordinate 0 (-1), Coordinate 1 0, Coordinate 0 1],
    Card "Eel" [Coordinate 1 (-1), Coordinate 0 1, Coordinate (-1) (-1)],
    Card "Cobra" [Coordinate 0 (-1), Coordinate 1 1, Coordinate (-1) 1]
  ]

superCard :: Card
superCard = Card "Super" [Coordinate 0 1, 
                          Coordinate 1 0, 
                          Coordinate 1 1, 
                          Coordinate (-1) 0, 
                          Coordinate (-1) 1, 
                          Coordinate 0 (-1), 
                          Coordinate 1 (-1), 
                          Coordinate (-1) (-1)]

findCard :: String -> Maybe Card
findCard name = find (\card -> cardName card == name) allCards

--------------------------------------------------------------------------------

-- | data types

--------------------------------------------------------------------------------

data Coordinate = Coordinate {x :: Int, y :: Int} deriving (Show, Eq)

data Card = Card
  { cardName :: String,
    -- | Coordinate from the pawn, i.e. not the actual position on the board
    possibleMoves :: [Coordinate]
  }
  deriving (Eq)

data GameState = GameState
  { -- | The current cards in game
    cards :: [Card],
    -- | The coordinates of player 1 or 2's pawns
    player1PawnCoordinates :: [Coordinate],
    player2PawnCoordinates :: [Coordinate],
    -- | Which player's turn is it, 0 for player 1, 1 for player 2
    turn :: Int,
    -- | Whether or not player 1 or 2 has used their super move
    player1HasUsedSuperMove :: Bool,
    player2HasUsedSuperMove :: Bool
  }

data GameMoves = GameMoves
  { card :: Card,
    sourcePosition :: Coordinate,
    destinationPosition :: Coordinate,
    isSuperMove :: Bool
  }
