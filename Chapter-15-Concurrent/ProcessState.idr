module ProcessState

import System.Concurrency.Channels


public export
data Fuel = Dry | More (Lazy Fuel)


data Message = Add Nat Nat


data MessagePID = MkMessage PID


data ProcState = Ready | Sent | Looping


data Process : Type -> ProcState -> ProcState -> Type where
  Request : MessagePID -> Message -> Process Nat st st
  Response : ((msg : Message) -> Process Nat Ready Ready) -> Process (Maybe Message) st Sent
  Spawn : Process () Ready Looping -> Process (Maybe MessagePID) st st
  Loop : Inf (Process a Ready Looping) -> Process a Sent Looping
  Action : IO a -> Process a st st
  Pure : a -> Process a st st
  (>>=) : Process a st1 st2 -> (a -> Process b st2 st3) -> Process b st1 st3

export partial
forever : Fuel
forever = More forever


total
run : Fuel -> Process t in_state out_state -> IO (Maybe t)
run fuel (Request (MkMessage process) msg) = do
  Just chan <- connect process
    | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok
    then do
      Just x <- unsafeRecv Nat chan
        | Nothing => pure Nothing
      pure $ Just x
    else
      pure Nothing

run fuel (Response f) = do
  Just sender <- listen 1
    | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv Message sender
    | Nothing => pure (Just Nothing)
  Just res <- run fuel $ f msg
    | Nothing => pure Nothing
  unsafeSend sender res
  pure (Just (Just msg))

run fuel (Spawn proc) = do
  Just pid <- spawn (do run fuel proc
                        pure ())
    | Nothing => pure (Just Nothing)
  pure (Just (Just (MkMessage pid)))

run (More fuel) (Loop proc) = run fuel proc
run fuel (Action action) = action >>= \res => pure $ Just res
run fuel (action >>= next) = do
  Just x <- run fuel action
    | Nothing => pure Nothing
  run fuel $ next x


Service : Type -> Type
Service a = Process a Ready Looping


Client : Type -> Type
Client c = Process c Ready Ready


procAdder : Service ()
procAdder = do
  Response (\msg => case msg of
                      Add x y => Pure (x + y))
  Loop procAdder


procMain : Client ()
procMain = do
  Just adder_id <- Spawn procAdder
    | Nothing => Action (putStrLn "Spawn failed")
  answer <- Request adder_id (Add 3 2)
  Action (printLn answer)


partial
runProc : Process () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()


main : IO ()
main = runProc procMain
