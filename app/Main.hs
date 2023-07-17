module Main where
import System.IO
import Data.Foldable

f .> g = g . f

type ErrorMsg = String
type Requirement = String -> [String] -> Maybe ErrorMsg
require :: Bool -> ErrorMsg -> Maybe ErrorMsg
require expected errorMsg =
   if expected
    then Nothing
    else Just errorMsg

requireNotTooShort word _ =
  require (2 <= length word) "A word must be 2 or more characters."

requireWordStartsWithLastCharOfLastUsedWord _ [] = Nothing
requireWordStartsWithLastCharOfLastUsedWord word (lastUsedWord:_) =
  require (head word == last lastUsedWord) (word ++ " does not start with " ++ [last lastUsedWord] ++ ".")

requireNotAlreadyUsed word usedWords =
  require (word `notElem` usedWords) (word ++ " is already used.")

requirements :: [String -> [String] -> Maybe ErrorMsg]
requirements = [
    requireNotTooShort,
    requireWordStartsWithLastCharOfLastUsedWord,
    requireNotAlreadyUsed
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
      (asum $ map (($ word) .> ($ usedWords)) requirements)

main = processTurn []
