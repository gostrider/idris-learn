module ProcessLib

import System.Concurrency.Channels

%default total


export
data MessagePID : (iface : reqType -> Type) -> Type where


public export
data ProcState = NoRequest | Sent | Complete


public export
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

public export
data Fuel


export partial
forever : Fuel


export
run : Fuel -> Process iface t in_state out_state -> IO (Maybe t)


public export
NoRecv : Void -> Type


public export
Service : (iface : reqType -> Type) -> Type -> Type


public export
Client : Type -> Type


export partial
runProc : Process iface () in_state out_state -> IO ()
