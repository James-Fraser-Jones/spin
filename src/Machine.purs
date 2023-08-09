module Machine where

import Prelude
import Effect (Effect)
import Data.Tuple
import Control.Monad.State
import Control.Lazy (fix)
import Data.Exists
import Data.Profunctor

---------------------------------------------------------------------

--Mealy machines are effectively a kleisi arrow from the State monad
--newtype Mealy i o s = Mealy (i -> State s o)
newtype Mealy s i o = Mealy (s -> i -> Tuple s o)
newtype Mealy' i o s = Mealy' (s -> i -> Tuple s o)

newtype Emealy i o = Emealy (Exists (Mealy i o))
newtype Fmealy i o = Fmealy (i -> Tuple (Fmealy i o) o)

---------------------------------------------------------------------

newtype Moore i o s = Moore (s -> Tuple (i -> s) o)
newtype Moore' s i o = Moore' (s -> Tuple (i -> s) o)

newtype Emoore i o = Emoore (Exists (Moore i o))
newtype Fmoore i o = Fmoore (Tuple (i -> Fmoore i o) o)

---------------------------------------------------------------------

instance Profunctor Emoore where
  dimap f g (Emoore e) = Emoore $ 
    flip runExists e $ 
      case _ of
        (Moore m) -> 
          mkExists $ Moore $ \s -> 
          case m s of
            Tuple u o -> Tuple (f >>> u) (g o)

-- instance Profunctor (Moore' s) where
--   dimap f g (Moore' m) = Moore' $ \s -> 
--     case m s of
--       Tuple u o -> Tuple (f >>> u) (g o)
