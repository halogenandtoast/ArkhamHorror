module Arkham.Treachery.Cards.RottingRemainsBloodOnTheAltar
  ( rottingRemainsBloodOnTheAltar
  , RottingRemainsBloodOnTheAltar(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Treachery.Attrs
import Arkham.Treachery.Cards.RottingRemains
import Arkham.Treachery.Runner

newtype RottingRemainsBloodOnTheAltar = RottingRemainsBloodOnTheAltar RottingRemains
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env, HasAbilities)

rottingRemainsBloodOnTheAltar :: TreacheryCard RottingRemainsBloodOnTheAltar
rottingRemainsBloodOnTheAltar = treachery
  (RottingRemainsBloodOnTheAltar . RottingRemains)
  Cards.rottingRemainsBloodOnTheAltar

instance TreacheryRunner env => RunMessage env RottingRemainsBloodOnTheAltar where
  runMessage msg (RottingRemainsBloodOnTheAltar inner) =
    RottingRemainsBloodOnTheAltar <$> runMessage msg inner
