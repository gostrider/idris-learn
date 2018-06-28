module ProcessLib

import System.Concurrency.Channels

%default total


export
data MessagePID : (iface : reqType -> Type) -> Type where
  MkMessage : PID -> MessagePID iface


public export
NoRecv : Void -> Type
NoRecv = const Void


public export
data ProcState = Ready | Sent | Looping


public export
data Process : (iface : reqType -> Type) -> Type ->
               (in_state : ProcState) -> (out_state : ProcState) -> Type where

  Request : MessagePID service_iface ->
            (msg : service_reqType) ->
            Process iface (service_iface msg) st st

  Respond : ((msg : reqType) -> Process iface (iface msg) Ready Ready) ->
            Process iface (Maybe reqType) st Sent

  Spawn : Process service_iface () Ready Looping ->
          Process iface (Maybe (MessagePID service_iface)) st st

  Loop : Inf (Process iface a Ready Looping) ->
         Process iface a Sent Looping

  Action : IO a -> Process iface a st st

  Pure : a -> Process iface a st st

  (>>=) : Process iface a st1 st2 ->
          (a -> Process iface b st2 st3) -> Process iface b st1 st3

public export
data Fuel = Dry | More (Lazy Fuel)


export partial
forever : Fuel
forever = More forever


export
run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run fuel (Request {service_iface} (MkMessage process) msg) = do
  Just chan <- connect process
    | _ => pure Nothing
  ok        <- unsafeSend chan msg
  if ok
    then do
      Just x <- unsafeRecv (service_iface msg) chan
        | Nothing => pure Nothing
      pure (Just x)
    else
      pure Nothing

run fuel (Respond {reqType} f) = do
  Just sender <- listen 1
    | Nothing => pure (Just Nothing)

  Just msg    <- unsafeRecv reqType sender
    | Nothing => pure (Just Nothing)

  Just res    <- run fuel $ f msg
    | Nothing => pure Nothing

  unsafeSend sender res
  pure $ Just $ Just msg

run fuel (Spawn proc) = do
  Just pid <- spawn (do run fuel proc
                        pure ())
    | Nothing => pure (Just Nothing)
  pure $ Just $ Just $ MkMessage pid

run (More fuel) (Loop proc)       = run fuel proc
run fuel        (Action action)   = action >>= \res => pure $ Just res
run fuel        (Pure x)          = pure $ Just x
run fuel        (action >>= next) = do Just action' <- run fuel action
                                         | Nothing => pure Nothing
                                       run fuel $ next action'
run Dry         _                 = pure Nothing


public export
Service : (iface : reqType -> Type) -> Type -> Type
Service iface x = Process iface x Ready Looping


public export
Client : Type -> Type
Client x = Process NoRecv x Ready Ready


export partial
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()
