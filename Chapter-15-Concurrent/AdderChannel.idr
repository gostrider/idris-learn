module AdderChannel

import System.Concurrency.Channels

data Message = Add Nat Nat


adder : IO ()
adder = do
  -- Cannot reuse the channel
  Just sender_chan <- listen 1
    | Nothing => adder
  Just msg <- unsafeRecv Message sender_chan
    | Nothing => adder

  case msg of
    Add x y => do
      ok <- unsafeSend sender_chan (x + y)
      adder


main : IO ()
main = do
  -- spwan process
  Just adder_id <- spawn adder
    | Nothing => putStrLn "Spawn failed"

  Just chan <- connect adder_id
    | Nothing => putStrLn "Connection failed"

  ok <- unsafeSend chan (Add 2 3)

  -- Recevice message type not guarantee
  Just answer <- unsafeRecv Nat chan
    | Nothing => putStrLn "Send failed"

  printLn answer
