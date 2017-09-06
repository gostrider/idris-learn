module TreeLabelState

-- import Control.Monad.State

data State : (stateType : Type) -> Type -> Type where
  Get : State stateType stateType
  Put : stateType -> State stateType ()

  Pure : ty -> State stateType ty
  Bind : State stateType a -> (a -> State stateType b) -> State stateType b


get : State stateType stateType
get = Get


put : stateType -> State stateType ()
put = Put


pure : ty -> State stateType ty
pure = Pure

(>>=) : State stateType a -> (a -> State stateType b) -> State stateType b
-- (>>=) = Do


runState : State stateType a -> (st : stateType) -> (a, stateType)
runState Get st = (st, st)
runState (Put newState) st = ((), newState)
runState (Pure x) st = (x, st)
runState (Bind c f) st = let (val, newState) = runState c st
                              in runState (f val) newState


data Tree a = Empty
            | Node (Tree a) a (Tree a)


testTree : Tree String
testTree = Node (Node (Node Empty "jim" Empty)
                      "fred"
                      (Node Empty "sheila" Empty))
                "alice"
                (Node Empty "bob" (Node Empty "eve" Empty))


flatten : Tree a -> List a
flatten Empty = []
flatten (Node l v r) = flatten l ++ v :: flatten r


treeLabelWith : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith Empty = pure Empty
treeLabelWith (Node l v r) = do l_labelled <- treeLabelWith l
                                (this :: rest) <- get
                                put rest
                                r_labelled <- treeLabelWith r
                                pure (Node l_labelled (this, v) r_labelled)

-- Using Control.Monad.State
-- treeLabel : Tree a -> Tree (Integer, a)
-- treeLabel tree = evalState (treeLabelWith tree) [1..]


treeLabel : Tree a -> Tree (Integer, a)
treeLabel tree = runState (treeLabelWith tree) [1..]
