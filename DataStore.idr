module DataStore

import Data.Vect
import Data.String
import System.REPL

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

data Command
  = Add String
  | Get Integer
  | Search String
  | Size
  | Quit

size : DataStore -> Nat
size (MkData sz itms) = sz

items : (store : DataStore) -> Vect (size store) String
items (MkData sz itms) = itms

addToStore : DataStore -> String -> DataStore
addToStore (MkData sz itms) y = MkData _ $ addToData itms
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [y]
    addToData (x :: xs) = x :: addToData xs

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let store_items = items store in
    case integerToFin pos (size store) of
      Nothing => Just ("Out of range\n", store)
      Just id => Just (index id store_items ++ "\n", store)

command : (cmd : String) -> (args : String) -> Maybe Command
command "add" str = Just $ Add str
command "get" val = case all isDigit (unpack val) of
                        False => Nothing
                        True => Just $ Get (cast val)
command "search" str = Just $ Search str
command "size" str = Just Size
command "quit" "" = Just Quit
command _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
  (cmd, args) => command cmd (ltrim args)

withIndex : {n : Nat} -> Vect n x -> Vect n (Nat, x)
withIndex [] = []
withIndex (y :: xs) = case n of
  S( i ) => (i, y) :: withIndex xs
  Z      => (Z, y) :: withIndex xs

searchEntry : (query : String) -> (store : DataStore) -> Maybe (String, DataStore)
searchEntry query store =
  let numbered_items = withIndex $ reverse $ items store in
      case Vect.filter filterFn numbered_items of
        (nm ** resp) => Just (foldl transformFn "" resp, store)
  where
    filterFn : (Nat, String) -> Bool
    filterFn (_, x) = isPrefixOf query x

    transformFn : String -> (Nat, String) -> String
    transformFn acc (num, val) = acc ++ show num ++ ": " ++ val ++ "\n"

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
  = case parse inp of
    Nothing => Just ("Invalid Command\n", store)
    Just (Add itm)  => Just ("ID: " ++ show (size store) ++ "\n", addToStore store itm)
    Just (Get val)  => getEntry val store
    Just Size       => Just ("Size: " ++ show (size store) ++ "\n", store)
    Just (Search s) => searchEntry s store
    Just Quit       => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
