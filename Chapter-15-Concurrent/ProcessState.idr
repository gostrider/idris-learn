module ProcessState

import System.Concurrency.Channels


data Fuel = Dry | More (Lazy Fuel)


data Message = Add Nat Nat


data MessagePID = MkMessage PID


data ProcState = NoRequest | Sent | Complete


data Process : Type -> (in_state : ProcState) -> (out_state : ProcState) -> Type where
  Request : MessagePID -> Message -> Process Nat st st
  Response : ((msg : Message) -> Process Nat NoRequest NoRequest) -> Process (Maybe Message) st Sent
  Spawn : Process () NoRequest Complete -> Process (Maybe MessagePID) st st
  Loop : Inf (Process a NoRequest Complete) -> Process a Sent Complete
  Action : IO a -> Process a st st
  Pure : a -> Process a st st
  (>>=) : Process a st1 st2 -> (a -> Process b st2 st3) -> Process b st1 st3


Service : Type -> Type
Service a = Process a NoRequest Complete


Client : Type -> Type
Client c = Process c NoRequest NoRequest


run : Fuel -> Process t in_state out_state -> IO (Maybe t)
run fuel (Request (MkMessage process) msg) = do
  Just chan <- connect process | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok
    then do
      Just x <- unsafeRecv Nat chan | Nothing => pure Nothing
      pure $ Just x
    else
      pure Nothing

run fuel (Response f) = ?run_rhs_2
run fuel (Spawn x) = ?run_rhs_3
run fuel (Loop x) = ?run_rhs_4
run fuel (Action x) = ?run_rhs_5
run fuel (x >>= f) = ?run_rhs_6


procAdder : Service ()
procAdder = do
  Response (\msg => case msg of
                      Add x y => Pure (x + y))
  Loop procAdder


procMain : Client ()
procMain = do
  Just adder_id <- Spawn procAdder | Nothing => Action (putStrLn "Spawn failed")
  answer <- Request adder_id (Add 3 2)
  Action (printLn answer)
