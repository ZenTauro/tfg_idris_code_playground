module LengthIO

printLength : IO ()
printLength = getLine >>=
  \input => let len = length input in
    putStrLn $ show len

printLonger : IO ()
printLonger = do
  a <- getLine <&> length
  b <- getLine <&> length
  if a > b
    then putStrLn $ show a
    else putStrLn $ show b

main : IO ()
main = printLonger
