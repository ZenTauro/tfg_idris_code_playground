module Parser

import Data.Vect
import Data.Fin

data ParserState
     = Initial
     | Digit Nat
     | Letter
     | Invalid Nat
     | Valid

data Parser : (st : ParserState) -> Type where
     MkParser : Parser st

Show ParserState where
  show Initial = "Initial"
  show (Digit k) = "Digit " ++ (show k)
  show Letter = "Letter"
  show (Invalid k) = "Invalid " ++ (show k)
  show Valid = "Valid"

stepParse : (1 input : Vect (S k) Char) ->
            (state : Parser a) ->
            (Vect k Char, Parser b)
stepParse (x :: xs) state = (xs, MkParser)

stepValidInitial : Char -> ParserState
stepValidInitial c =
  if isDigit c
    then Digit 1
    else Invalid 1

initParse : (c : Char) -> Parser $ stepValidInitial c
initParse c = MkParser

stepValidDigit : Char -> Nat -> ParserState
stepValidDigit c k =
  if isDigit c
    then if k == 8
      then Letter
      else Digit (S k)
    else Invalid 1

digit : (1 x : Parser $ Digit k) ->
        (c : Char) ->
        Parser $ stepValidDigit c k
digit MkParser c = MkParser

stepValidChar : Char -> ParserState
stepValidChar c =
  if isAlpha c
    then Valid
    else Invalid 8

char : (1 x : Parser Letter) ->
       (c : Char) ->
       Parser $ stepValidChar c
char MkParser c = MkParser

runParse1 : {a : _} -> (1 f : Vect n Char) -> (state : Parser a) -> ParserState
runParse1 {a} [] MkParser = a
runParse1 {a} (x :: xs) state =
  case a of
    Initial   => runParse1 xs $ initParse x
    Digit k   => runParse1 xs $ digit state x
    Letter    => runParse1 xs $ char state x
    Invalid k => Invalid k
    Valid     => Valid

runParseInit : (1 f : Vect n Char) -> (state : Parser Initial) -> ParserState
runParseInit f state = runParse1 f state

runParse : (1 f : Vect n Char) -> ParserState
runParse f = runParseInit f MkParser

main : IO ()
main = do
  putStr "Escriba DNI: "
  x <- getLine
  printLn $ runParse $ fromList $ unpack x
