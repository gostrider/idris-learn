module Process

import System.Concurrency.Channels


data MessagePID = MkMessage PID
data Message = Add Nat Nat


data Process : Type -> Type where
  Action : IO a -> Process a
  Pure : a -> Process a
  Spawn : Process () -> Process (Maybe MessagePID)
  Request : MessagePID -> Message -> Process (Maybe Nat)
  Response : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
  (>>=) : Process a -> (a -> Process b) -> Process b


run : Process t -> IO t
run (Action act) = act
run (Pure x) = pure x
run (Spawn proc) = do
  -- explicit handle spawn process
  Just pid <- spawn $ run proc
    | Nothing => pure Nothing
  pure $ Just $ MkMessage pid
run (Request (MkMessage pid) msg) = do
  Just chan <- connect pid
    | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok then do
    Just x <- unsafeRecv Nat chan
      | Nothing => pure Nothing
    pure $ Just x
  else pure Nothing
run (Response f) = do
  Just sender <- listen 1
    | Nothing => pure Nothing

  Just msg <- unsafeRecv Message sender
    | Nothing => pure Nothing

  res <- run $ f msg
  unsafeSend sender res
  pure $ Just msg
run (x >>= f) = do
  x' <- run x
  run $ f x'


-- only accept 1 kind of message
clac : Message -> Process Nat
clac (Add x y) = Pure $ x + y


adder : Process ()
adder = do
  Response clac
  adder


procMain : Process ()
procMain = do
  Just adder_id <- Spawn adder
    | Nothing => Action (putStrLn "Spawn failed")
  Just ans <- Request adder_id $ Add 2 3
    | Nothing => Action (putStrLn "Request failed")
  Action $ printLn ans
