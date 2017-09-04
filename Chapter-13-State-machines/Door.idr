module Door


data DoorState = DoorClosed | DoorOpen

data DoorCmd : Type -> DoorState -> DoorState -> Type where
  Open     : DoorCmd () DoorClosed DoorOpen
  Close    : DoorCmd () DoorOpen   DoorClosed
  RingBell : DoorCmd () DoorClosed DoorClosed

  Pure : ty -> DoorCmd ty state state
  (>>=) : DoorCmd a st1 st2 -> (a -> DoorCmd b st2 st3) -> DoorCmd b st1 st3


doorProg : DoorCmd () DoorClosed DoorClosed
doorProg = do RingBell
              Open
              Close
