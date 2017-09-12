module ProcessIFace

import System.Concurrency.Channels


data Message = Add Nat Nat


data ProcState = NoRequest | Sent | Complete


data Fuel = Dry | More (Lazy Fuel)


data MessagePID  : (iface : reqType -> Type) -> Type where
  MkMessage : PID -> MessagePID iface


AdderType : Message -> Type
AdderType (Add x y) = Nat


data Process : (iface : reqType -> Type) -> Type ->
               (in_state : ProcState) ->
               (out_state : ProcState) ->
               Type where

  Request : MessagePID service_iface ->
            (msg : service_reqType) ->
            Process iface (service_iface msg) st st

  Respond : ((msg : reqType) -> Process iface (iface msg) NoRequest NoRequest) ->
            Process iface (Maybe reqType) st Sent

  Spawn : Process service_iface () NoRequest Complete ->
          Process iface (Maybe (MessagePID service_iface)) st st

  Loop : Inf (Process iface a NoRequest Complete) ->
         Process iface a Sent Complete

  Action : IO a -> Process iface a st st

  Pure : a -> Process iface a st st

  (>>=) : Process iface a st1 st2 ->
          (a -> Process iface b st2 st3) -> Process iface b st1 st3


procAdder : Process AdderType () NoRequest Complete


NoRecv : Void -> Type
NoRecv = const Void


procMain : Process NoRecv () NoRequest NoRequest


Service : (iface : reqType -> Type) -> Type -> Type


run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)
run fuel (Request {service_iface} (MkMessage process) msg) = do
  Just chan <- connect process | _ => pure Nothing
  ok <- unsafeSend chan msg
  if ok
    then do
      Just x <- unsafeRecv (service_iface msg) chan | Nothing => pure Nothing
      pure $ Just x
    else
      pure Nothing
run fuel (Respond {reqType} f) = do
  Just sender <- listen 1 | Nothing => pure (Just Nothing)
  Just msg <- unsafeRecv reqType sender | Nothing => pure (Just Nothing)
  Just res <- run fuel (f msg) | Nothing => pure Nothing
  unsafeSend sender res
  pure (Just (Just msg))

run fuel (Spawn x) = ?run_rhs_3
run fuel (Loop x) = ?run_rhs_4
run fuel (Action x) = ?run_rhs_5
run fuel (Pure x) = ?run_rhs_6
run fuel (x >>= f) = ?run_rhs_7


partial
forever : Fuel
forever = More forever


partial
runProc : Process iface () in_state out_state -> IO ()
runProc proc = do run forever proc
                  pure ()
