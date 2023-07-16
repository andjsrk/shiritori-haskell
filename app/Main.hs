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
    "The word must be 2 or more characters."

checkWordStartsWithLastCharOfLastUsedWord _ [] = Nothing
checkWordStartsWithLastCharOfLastUsedWord word (lastUsedWord:_) =
  (head word == last lastUsedWord) `isFalseThen`
    (word ++ " does not starts with " ++ [last lastUsedWord] ++ ".")

checkAlreadyUsed word usedWords =
  (word `notElem` usedWords) `isFalseThen`
    (word ++ " is already used.")

checks :: [String -> [String] -> Maybe ErrorMsg]
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
    maybe
      (processTurn (word:usedWords))
      (\msg -> do
        putStrLn msg
        processTurn usedWords)
      (asum $ map (($ word) .> ($ usedWords)) checks)

main = processTurn []
