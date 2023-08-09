module MachineNew where

import Prelude
import Effect (Effect)
import Data.Tuple
import Data.List

--------------------------------------------------------------

newtype Moore s i o = Moore (s -> Tuple (i -> s) o)

liftI :: ∀ i o. (i -> o) -> Moore i i o
liftI f = Moore $ \i -> Tuple identity (f i)

liftO :: ∀ i o. (i -> o) -> Moore o i o
liftO f = Moore $ \o -> Tuple f o

mapS :: ∀ s i o t. (t -> s) -> (s -> t) -> Moore s i o -> Moore t i o
mapS f g (Moore m) = Moore $ \t ->
  case (f >>> m) t of
    Tuple u o -> Tuple (u >>> g) o

mapI :: ∀ s i o j. (j -> i) -> Moore s i o -> Moore s j o
mapI f (Moore m) = Moore $ \s ->
  case m s of
    Tuple u o -> Tuple (f >>> u) o

mapO :: ∀ s i o p. (o -> p) -> Moore s i o -> Moore s i p
mapO g (Moore m) = Moore $ \s -> 
  case m s of
    Tuple u o -> Tuple u (g o)

joinSerial :: ∀ s i o t p. Moore s i o -> Moore t o p -> Moore (Tuple s t) i p

joinParallel :: ∀ s i o t j p. Moore s i o -> Moore t j p -> Moore (Tuple s t) (Tuple i j) (Tuple o p)

joinSelf :: 

--------------------------------------------------------------

newtype Moore' i o = Moore' (Tuple (i -> Moore' i o) o)

init :: ∀ s i o. Moore s i o -> s -> Moore' i o
init (Moore m) s = Moore' $ case m s of
  Tuple u o -> Tuple (\i -> init (Moore m) (u i)) o

run :: ∀ i o. Moore' i o -> List i -> List o
run _ Nil = Nil
run (Moore' (Tuple u o)) (Cons i is) = Cons o (run (u i) is)