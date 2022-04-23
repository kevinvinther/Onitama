module Main where

import qualified Lib
import Options.Applicative
import Data.Semigroup ((<>))

data Command
  = SimulateGame String
  | GenerateGame Integer Integer
  | CountGames Integer String

data Args = Args
  { file :: Maybe String
  , cmd :: Command
  }

fileParser = strOption
  ( long "file"
  <> short 'f'
  <> help "File path for saving the output"
  <> metavar "FILEPATH" )

cmdParser = subparser
  ( command "simulateGame" (info simulateGame (progDesc "Call simulateGame"))
  <> command "generateGame" (info generateGame (progDesc "Call generateGame"))
  <> command "countGames" (info countGames (progDesc "Call countGames"))
  ) where
    simulateGame = SimulateGame
      <$> argument str (metavar "FILEPATH" <> help "Input File")
    generateGame = GenerateGame
      <$> argument auto (metavar "INT" <> help "Seed")
      <*> argument auto (metavar "INT" <> help "Max depth")
    countGames = CountGames
      <$> argument auto (metavar "INT" <> help "Max depth")
      <*> argument str (metavar "FILEPATH" <> help "Input File")

argsParser :: Parser Args
argsParser = Args <$> optional fileParser <*> cmdParser

main :: IO ()
main = do
  args <- execParser $ info (argsParser <**> helper) fullDesc
  str <- case cmd args of
    SimulateGame x -> Lib.simulateGame x
    GenerateGame x y -> return $ Lib.generateGame x y
    CountGames x y -> Lib.countGames x y
  case file args of
    Nothing -> putStrLn str
    Just s -> writeFile s str
