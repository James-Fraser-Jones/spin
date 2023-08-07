module Machine
  ( GlossPlayIO(..)
  , Mealy
  , Moore
  , Time
  , fnToMealy
  , fnToMoore
  , mooreToMealy
  )
  where

-----------------------------------------------------------------------------------

import Prelude
import Effect (Effect)
import Data.Tuple

-----------------------------------------------------------------------------------

type Time = Number

--inspiration from Haskell's Gloss library
newtype GlossPlayIO = GlossPlayIO (∀ settings world picture event .
  settings
  -> (world -> Effect picture)
  -> (event -> world -> Effect world)
  -> (Time -> world -> Effect world)
  -> Effect Unit
  )

newtype Mealy s i o = Mealy {state :: s, update :: Tuple s i -> Tuple s o}

newtype Moore s i o = Moore {state :: s, update :: s -> i -> s, respond :: s -> o}

newtype Mealy' s i o = Mealy' {state :: s, update :: s -> i -> Tuple s o}

newtype Moore' s i o = Moore' {state :: s, update :: s -> Tuple (i -> s) o}

--equivalent to the state Monad: a -> m b (where m = State s)
--type Mealy'' s i o = i -> State s o

type Moore'' s i o = s -> Tuple (i -> s) o
--can we form a monad instance for this? (no probably not)
--can we form *Arrow* ahem, profunctor instance for this? c:

--state pushed into definition somehow???
newtype Mealy''' i o = Mealy''' (
  i -> Tuple (Mealy''' i o) o
)

newtype Moore''' i o = Moore''' (
  Tuple (i -> Moore''' i o) o
)
--has profucntor instance as long as "o" is a pointed type (has minimum 1 designated inhabitant)

-----------------------------------------------------------------------------------

--proves all Moore Machines are Mealy machines (i.e. injective mapping)
mooreToMealy :: ∀ s i o. Moore s i o -> Mealy s i o
mooreToMealy (Moore {state: s, update: u, respond: r}) = 
  Mealy { 
    state: s
  , update: case _ of (Tuple s i) -> Tuple (u s i) (r s) 
  }

--proves all functions are Mealy machines (that have no state)
fnToMealy :: ∀ a b. (a -> b) -> Mealy Unit a b
fnToMealy f = 
  Mealy {
    state: unit
  , update: case _ of (Tuple _ a) -> Tuple unit (f a)
  }

--proves all functions are Moore machines
--provided the Moore machine has enough memory to store the input
--and is given an initial value to hold
fnToMoore :: ∀ a b. (a -> b) -> a -> Moore a a b
fnToMoore f a = 
  Moore {
    state: a
  , update: flip const
  , respond: f
  }

-----------------------------------------------------------------------------------

--external API for Machines (not explicitly recognizing internal state, but admitting the possibility)
class Machine m where
  apply :: ∀ i o. (m i o) -> i -> Tuple (m i o) o 

--Mealy machines (with arbitrary state) are Machines
instance Machine (Mealy s) where
  apply (Mealy m@{state: s, update: u}) i = 
    case (curry u s i) of 
    (Tuple s' o) -> 
      let m' = m {state = s'} in 
      Tuple (Mealy m') o

--Moore machines are also Machines
instance Machine (Moore s) where
  apply (Moore m@{state: s, update: u, respond: r}) i =
    let s' = u s i
        o  = r s'
        m' = m {state = s'}
     in Tuple (Moore m') o

--functions are trivially Machines
instance Machine (->) where
  apply f a = Tuple f (f a)

-----------------------------------------------------------------------------------

--this doesn't work because:
--compose :: forall b c d. a c d -> a b c -> a b d
--we have "a = Moore s" but also "a = Moore s'" and "a = Moore (Tuple s s')"
-- instance Semigroupoid (Moore s) where
--   --compose :: ∀ a b c s s'. Moore s b c -> Moore s' a b -> Moore (Tuple s s') a c
--   compose (Moore {state: s2, update: u2, respond: r2}) (Moore {state: s, update: u, respond: r}) = 
--     Moore {
--       state: Tuple s s2
--     , update: \t a -> case t of 
--         (Tuple s s2) -> 
--           \a ->
--           let s'  = u s a
--               b   = r s'
--               s2' = u2 s2 b
--            in Tuple s' s2'
--     , respond: case _ of
--         (Tuple _ s2) -> 
--           r2 s2
--     }

--(g . f) x = g $ (f $ x)

-- instance (Machine m) => Semigroupoid m where
--   compose mbc mab = mac where
--     mac = 

--instance Machine

--vert :: Moore s a b -> Moore s' b c -> Moore (Tuple s s') a c
--horz :: Moore s a b -> Moore s' c d -> Moore (Tuple s s') (Tuple a c) (Tuple b d)