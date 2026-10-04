{- | Consternation on the Constellation's three encounter sets.

The scenario ships its own @Deep Ones@ set -- eight cards by fan artists, with
its own claw icon -- which has nothing in common with the official Innsmouth set
of the same name beyond the title. The core 'Arkham.EncounterSet.DeepOnes' is
hidden here so that @Set.DeepOnes@ means this one at every call site in the
campaign; anything needing the official set imports 'Arkham.EncounterSet'
directly.
-}
module Arkham.Homebrew.ConsternationOnTheConstellation.Sets (
  module Arkham.EncounterSet,
  pattern ConsternationOnTheConstellation,
  pattern DeepOnes,
  pattern SinkingShip,
) where

import Arkham.EncounterSet hiding (DeepOnes)

pattern ConsternationOnTheConstellation :: EncounterSet
pattern ConsternationOnTheConstellation =
  Homebrew ":consternation-on-the-constellation:consternation_on_the_constellation"

pattern DeepOnes :: EncounterSet
pattern DeepOnes = Homebrew ":consternation-on-the-constellation:deep_ones"

pattern SinkingShip :: EncounterSet
pattern SinkingShip = Homebrew ":consternation-on-the-constellation:sinking_ship"
