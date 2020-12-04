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

main : IO ()
main =
  connect $ \s =>
  let isOk # s = login s "somePass" in
    if isOk
      then
        do let secret # s = readSecret s
           let s = logout s
           disconnect s
           putStrLn secret
           pure ()
      else disconnect s
