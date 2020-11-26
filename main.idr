module main

namespace Linear
  export
  (>>=) : (1 act : IO a) -> (1 k : a -> IO b) -> IO b
  (>>=) = io_bind

data Access
  = LoggedOut
  | LoggedIn

data Store : Access -> Type
  where
  MkStore : (secret : String) -> Store st

connect : (1 _ : (1 _ : Store LoggedOut) -> IO a) -> IO a
login : (1 _ : Store LoggedOut) -> (password : String) ->
        Res Bool (\b => Store (if b then LoggedIn else LoggedOut))
logout : (1 _ : Store LoggedIn) -> Store LoggedOut
readSecret : (1 _ : Store LoggedIn) ->
             Res String (const (Store LoggedIn))
disconnect : (1 _ : Store LoggedOut) -> IO ()

storeProg : IO ()
storeProg =
    connect $ \s =>
    let
      res = login s "somePass"
    in
    case res of
      (val # r) => ?help

