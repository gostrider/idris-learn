module State

import Control.Monad.State

-- evalState returns computation result
-- execState returns final state
-- runState returns (state, result)

increase : Nat -> State Nat ()
increase k = do curr <- get
                put (curr + k)
