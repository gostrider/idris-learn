module DoorJam

data DoorState = DoorClosed
               | DoorOpen


data DoorResult = OK
                | Jammed


isDoorJam : DoorResult -> DoorState
isDoorJam OK = DoorOpen
isDoorJam Jammed = DoorClosed


data DoorCmd : (ty : Type) -> DoorState -> (ty -> DoorState) -> Type where
  Open : DoorCmd DoorResult DoorClosed DoorJam.isDoorJam
  Close    : DoorCmd () DoorOpen   (const DoorClosed)
  RingBell : DoorCmd () DoorClosed (const DoorClosed)
  Display  : String ->
             DoorCmd () state      (const state)

  Pure : (res : ty) -> DoorCmd ty (state_fn res) state_fn

  (>>=) : DoorCmd a state1 state2_fn ->
          ((res : a) -> DoorCmd b (state2_fn res) state3_fn) ->
          DoorCmd b state1 state3_fn


doorProg : DoorCmd () DoorClosed (const DoorClosed)
doorProg = do
  RingBell
  OK <- Open | Jammed => Display "Door jammed"
  Display "Door opened"
  -- Same as:
  -- jam <- Open
  -- (case jam of
  --       OK => do Display "Door opened"
  --                Close
  --       Jammed => Display "Door jammed")
  Close
