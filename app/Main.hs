module Main where
import System.IO

type ErrorMsg = String
type CheckResult = Either ErrorMsg ()
noProblem :: CheckResult
noProblem = Right ()
thereIs :: ErrorMsg -> CheckResult
thereIs = Left
expected `isFalseThen` errorMsg =
  if expected
    then noProblem
    else thereIs errorMsg

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

checks :: [String -> [String] -> CheckResult]
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
    either
      (\msg -> do
        putStrLn msg
        processTurn usedWords)
      (\_ -> processTurn (word:usedWords))
      (foldl (*>) noProblem $ map (($ usedWords) . ($ word)) checks)

main = processTurn []
