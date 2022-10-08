module Main where
import System.IO
import Control.Applicative

type Then a b = (a -> b)
type Else a b = (Maybe a -> b)
ifJust :: Maybe a -> Then a (f b) -> Else a (f b) -> f b
ifJust Nothing _ else_ = else_ Nothing
ifJust (Just x) then_ _ = then_ x

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

checkWordStartsWithLastCharOfLastUsedWord word [] = noProblem
checkWordStartsWithLastCharOfLastUsedWord word (lastUsedWord:_) =
  (head word == last lastUsedWord) `isFalseThen`
    (word ++ " does not starts with " ++ [last lastUsedWord] ++ ".")

checkAlreadyUsed word usedWords =
  (word `notElem` usedWords) `isFalseThen`
    (word ++ " is already used.")

checks = [
    checkTooShort,
    checkWordStartsWithLastCharOfLastUsedWord,
    checkAlreadyUsed
  ]

processTurn usedWords = do
  putStr " > "
  hFlush stdout
  word <- getLine
  if word == ":exit" then return ()
  else
    ifJust (foldl (<|>) noProblem (map (\check -> check word usedWords) checks))
      (\msg -> do
        putStrLn msg
        processTurn usedWords)
      (\_ ->
        processTurn (word:usedWords))

main = do
  processTurn []
