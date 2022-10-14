module Main where
import System.IO
import Data.Monoid
import Data.Maybe

type Then a b = (a -> b)
type Else b = b
ifM :: (Monad m, Eq (m a), Monoid (m a)) => m a -> Then a b -> Else b -> m b
ifM x th el
  | x == mempty = return el
  | otherwise   = th <$> x

type ExpectationFailedMsg = String
noProblem :: Maybe ExpectationFailedMsg
noProblem = Nothing
thereIs :: ExpectationFailedMsg -> Maybe ExpectationFailedMsg
thereIs = Just
expected `isFalseThen` msg =
  if expected
    then noProblem
    else thereIs msg

checkTooShort word _ =
  (2 <= length word) `isFalseThen`
    "The word must be 2 or more characters."

checkWordStartsWithLastCharOfLastUsedWord _ [] = noProblem
checkWordStartsWithLastCharOfLastUsedWord word (lastUsedWord:_) =
  (head word == last lastUsedWord) `isFalseThen`
    (word ++ " does not starts with " ++ [last lastUsedWord] ++ ".")

checkAlreadyUsed word usedWords =
  (word `notElem` usedWords) `isFalseThen`
    (word ++ " is already used.")

checks :: [String -> [String] -> Maybe ExpectationFailedMsg]
checks = [
    checkTooShort,
    checkWordStartsWithLastCharOfLastUsedWord,
    checkAlreadyUsed
  ]

processTurn usedWords = do
  putStr " > "
  hFlush stdout
  word <- getLine
  if word == ":q" then return ()
  else
    fromJust $ ifM (getFirst . mconcat $ map (First . \check -> check word usedWords) checks)
      (\msg -> do
        putStrLn msg
        processTurn usedWords)
      (processTurn (word:usedWords))

main = processTurn []
