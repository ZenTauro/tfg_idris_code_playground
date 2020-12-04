module Parser

import Data.Vect
import Data.Fin

data ParserState
     = Initial
     | Digit Nat
     | Letter
     | Invalid Nat
     | Valid

data Parser : ParserState -> Type where
     MkParser : Parser st

runParse : (1 _ : (1 _ : Parser Initial) -> IO a) -> IO a
runParse f = ?runParse_rhs

initParse : (c : Char) ->
            Parser $ if isDigit c
                        then Digit 1
                        else Invalid 1
initParse c = MkParser


digit : (1 x : Parser $ Digit k) ->
        (c : Char) ->
        Parser $ if isDigit c
                   then if k == 8
                     then Letter
                     else Digit (S k)
                   else Invalid 1
digit MkParser c = MkParser

char : (1 x : Parser Letter) ->
       (c : Char) ->
       Parser $ if isAlpha c
                   then Valid
                   else Invalid 8
char MkParser c = MkParser
