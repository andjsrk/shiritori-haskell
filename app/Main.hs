module Main where
import System.IO
import Data.Foldable

f .> g = g . f

type ErrorMsg = String
isFalseThen :: Bool -> ErrorMsg -> Maybe ErrorMsg
expected `isFalseThen` errorMsg =
  if expected
    then Nothing
    else Just errorMsg

checkTooShort word _ =
  (2 <= length word) `isFalseThen`
    "A word must be 2 or more characters."

checkWordDoesNotStartWithLastCharOfLastUsedWord _ [] = Nothing
checkWordDoesNotStartWithLastCharOfLastUsedWord word (lastUsedWord:_) =
  (head word == last lastUsedWord) `isFalseThen`
    (word ++ " does not start with " ++ [last lastUsedWord] ++ ".")

checkAlreadyUsed word usedWords =
  (word `notElem` usedWords) `isFalseThen`
    (word ++ " is already used.")

checks :: [String -> [String] -> Maybe ErrorMsg]
checks = [
    checkTooShort,
    checkWordDoesNotStartWithLastCharOfLastUsedWord,
    checkAlreadyUsed
  ]

processTurn usedWords = do
  putStr " > "
  hFlush stdout
  word <- getLine
  if word == ":q" then return ()
  else
    maybe
      (processTurn (word:usedWords))
      (\msg -> do
        putStrLn msg
        processTurn usedWords)
      (asum $ map (($ word) .> ($ usedWords)) checks)

main = processTurn []
