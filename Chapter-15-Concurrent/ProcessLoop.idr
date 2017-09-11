module ProcessLoop

import System.Concurrency.Channels


data Message = Add Nat Nat


data MessagePID = MkMessage PID


data Process : Type -> Type where
  Request : MessagePID -> Message -> Process (Maybe Nat)
  Respond : ((msg : Message) -> Process Nat) -> Process (Maybe Message)
  Spawn : Process () -> Process (Maybe MessagePID)
  Loop : Inf (Process a) -> Process a
  Action : IO a -> Process a
  Pure : a -> Process a
  (>>=) : Process a -> (a -> Process b) -> Process b


data Fuel = Dry | More (Lazy Fuel)


procAdder : Process ()
procAdder = do
  Respond (\msg => case msg of
                     Add x y => Pure (x + y))
  Loop procAdder


run : Fuel -> Process t -> IO (Maybe t)
run Dry _ = pure Nothing
run fuel (Request (MkMessage process) msg) = do
  Just chan <- connect process | _ => pure (Just Nothing)
  ok <- unsafeSend chan msg
  if ok
    then do
      Just x <- unsafeRecv Nat chan | Nothing => pure (Just Nothing)
      pure $ Just $ Just x
    else pure (Just Nothing)

run fuel (Respond calc) = do
  Just sender <- listen 1               | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv Message sender | Nothing => pure (Just Nothing)
  Just res <- run fuel $ calc msg       | Nothing => pure (Just Nothing)
  unsafeSend sender res
  pure $ Just $ Just msg

run fuel (Spawn proc) = do
  Just pid <- spawn (do run fuel proc
                        pure ())
    | Nothing => pure Nothing
  pure $ Just $ Just $ MkMessage pid

run (More fuel) (Loop act) = run fuel act
run fuel (Action act) = act >>= (\res => pure $ Just res)
run fuel (Pure val) = pure (Just val)
run fuel (c >>= f) = do
  Just x <- run fuel c | Nothing => pure Nothing
  run fuel $ f x

partial
forever : Fuel
forever = More forever

partial
runProc : Process () -> IO ()
runProc proc = do run forever proc
                  pure ()
