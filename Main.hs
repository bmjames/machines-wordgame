{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Exception         (catch, throwIO)
import Control.Monad.Trans.Class (lift)
import Data.Char                 (isLetter)
import Data.List                 (intersperse)
import Data.Machine
import System.IO
import System.IO.Error           (isEOFError)
import Text.Printf               (printf)


hCharsIn :: Handle -> SourceT IO Char
hCharsIn h = construct go where
  go = do
    c <- lift $ catch (fmap Just $ hGetChar h) (\e ->
      if isEOFError e then return Nothing else throwIO e)
    case c of
      Just c  -> yield c >> go
      Nothing -> stop

hLinesOut :: Handle -> ProcessT IO String ()
hLinesOut = autoM . hPutStrLn

type GameState = [Char]

data GameOutput = Start GameState
                | Correct GameState
                | Incorrect GameState
                | AlreadyGuessed Char
                | Won
                | Lost
                  deriving (Eq, Ord, Show)

showProgress :: String -> GameOutput -> String
showProgress word out = case out of
  Start s     -> showState s
  Correct s   -> unlines ["Ding!", showState s]
  Incorrect s -> unlines ["BZZT.", showState s]
  AlreadyGuessed c -> printf "You've already tried '%c'!" c
  Won         -> "Well done!"
  Lost        -> "You're out of turns. Better luck next time."

  where
    showState cs  = intersperse ' ' $ map (showChar cs) word
    showChar cs c = if c `elem` cs then c else '_'

game :: Int -> String -> Process Char GameOutput
game n word = construct $ do
  yield $ Start initState
  go initState

  where
    go s
      | all (`elem` s) word     = yield Won
      | incorrectGuesses s >= n = yield Lost
      | otherwise               = makeGuess s

    makeGuess s = do
      c <- await
      if c `elem` s
      then do
        yield $ AlreadyGuessed c
        go s
      else let s' = c:s in do
        yield $ if c `elem` word
                then Correct s'
                else Incorrect s'
        go s'

    initState = []
    incorrectGuesses s = length $ filter (not . (`elem` word)) s

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  let word = "haskell"
  runT_ $
    hCharsIn stdin
       ~> filtered isLetter
       ~> game 3 word
       ~> auto (showProgress word)
       ~> hLinesOut stdout
