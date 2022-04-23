module Spec where

import Control.Exception
import Control.Monad
import Data.Either
import Data.List (intercalate)
import Data.Maybe
import System.IO
import System.Directory (getCurrentDirectory, listDirectory)
import System.FilePath.Posix ((</>), takeBaseName)

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Lib

-- Strip newlines and whitespace from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- Check if a string contains at least one of the strings as a substring.
hasSubstring :: String -> [String] -> Bool
hasSubstring x xs = any (T.isInfixOf . T.pack $ x) $ map T.pack xs

unique :: Ord a => [a] -> [a]
unique = S.toList . S.fromList

-- Retrieve the stems (the path without the extension) of all of the files in
-- the directory.
listStems :: String -> IO ([String])
listStems dir = liftM f $ listDirectory dir
  where f = map (dir </>) . unique . (filter (not . null)) . map takeBaseName

-- Forcefully evaluate an IO action to force any exceptions to propagate.
strictTry :: IO a -> IO (Either SomeException a)
strictTry expr = try $ expr >>= evaluate

-- Format a string nicely if it contains multiple lines.
formatMultiline :: String -> String
formatMultiline str
  | length ls == 1 = head ls
  | otherwise = "\n" ++ (intercalate "\n" $ map ("  " ++) ls)
  where ls = lines $ strip str

-- Format the test's information.
formatTest
  :: Maybe String -> Maybe String -> Either SomeException String
  -> Maybe String -> String
formatTest input params obtained expected = intercalate "\n" $ catMaybes
  [ ("Input: " ++) . formatMultiline <$> input
  , ("Params: " ++) . formatMultiline <$> params
  , ("Expected: " ++) . formatMultiline <$> expected
  , let s = either displayException id obtained in
      Just . ("Obtained: " ++) . formatMultiline $ s ]

check :: (b -> Bool) -> Either a b -> Bool
check _ (Left _) = False
check f (Right x) = f x

-- Check a test for correctness and report the result.
report
  :: String -> Maybe String -> Maybe String -> Either SomeException String
  -> String -> IO ()
report test input params obtained expected = do
  let name = takeBaseName test
  if check ((== strip expected) . strip) obtained then do
    putStrLn $ "Passed test " ++ name
  else do
    putStrLn $ "Failed test " ++ name
    putStrLn $ formatTest input params obtained (Just expected)
  putStrLn ""

testSimulateGame :: String -> IO ()
testSimulateGame test = do
  let input = test ++ ".in"
      output = test ++ ".out"
  s1 <- readFile input
  s2 <- strictTry $ Lib.simulateGame input
  s3 <- readFile output
  report test (Just s1) Nothing s2 s3

testGenerateGame :: String -> IO ()
testGenerateGame test = do
  let name = takeBaseName test
      param = test ++ ".param"
      output = test ++ ".out"
  s1 <- readFile param
  let [seed, maxDepth] = map read . words . strip $ s1
  s2 <- strictTry . return $ Lib.generateGame seed maxDepth
  s3 <- readFile output
  s4 <- strictTry $ Lib.simulateGame output
  let f1 = check ((== strip s3) . strip) s2
      f2 = not $ check (`hasSubstring` ["InvalidMove", "InvalidFormat"]) s4
  if f1 && f2 then do
    putStrLn $ "Passed test " ++ name
  else do
    putStrLn $ "Failed test " ++ name
    unless f1 $ putStrLn $ formatTest Nothing (Just s1) s2 (Just s3)
    unless f2 $ putStrLn "The randomly generated moves are not valid."
  putStrLn ""

testCountGames :: String -> IO ()
testCountGames test = do
  let input = test ++ ".in"
      param = test ++ ".param"
      output = test ++ ".out"
  s1 <- readFile input
  s2 <- readFile param
  let maxDepth = read $ strip s2
  s3 <- strictTry $ Lib.countGames maxDepth input
  s4 <- readFile output
  report test (Just s1) (Just s2) s3 s4

funcs :: [(String, String -> IO ())]
funcs =
  [ ("simulateGame", testSimulateGame)
  , ("generateGame", testGenerateGame)
  , ("countGames", testCountGames) ]

box :: Int -> String -> String
box n s = b ++ "\n\n" ++ p ++ s ++ "\n\n" ++ b
  where
    b = replicate n '#'
    p = replicate ((n - length s) `div` 2) ' '

main :: IO ()
main = do
  dir <- getCurrentDirectory
  forM_ funcs $ \(name, f) -> do
    tests <- listStems $ dir </> "unit_tests" </> name
    let names = map takeBaseName tests
    putStrLn $ box 30 name
    putStrLn ""
    mapM_ f tests
