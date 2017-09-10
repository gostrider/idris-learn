module Process

import System.Concurrency.Channels

data MessagePID = MkMessage PID

data Process : Type -> Type where
  Action : IO a -> Process a
  Pure : a -> Process a
  (>>=) : Process a -> (a -> Process b) -> Process b
  Spawn : Process () -> Process (Maybe MessagePID)
  Request : MessagePID -> Message -> Process (Maybe Nat)


-- run : Process t -> IO t
-- run (Action x) = x
-- run (Pure x) = pure x
-- run (x >>= f) = do x' <- run x
--                    run $ f x'
-- run (Spawn proc) = do Just pid <- spawn $ run proc | Nothing => pure Nothing
--                       pure $ Just $ MkMessage pid
