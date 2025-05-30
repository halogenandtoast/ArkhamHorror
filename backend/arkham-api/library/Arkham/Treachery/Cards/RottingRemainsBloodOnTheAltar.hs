module Arkham.Treachery.Cards.RottingRemainsBloodOnTheAltar (rottingRemainsBloodOnTheAltar) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.RottingRemains
import Arkham.Treachery.Import.Lifted

newtype RottingRemainsBloodOnTheAltar = RottingRemainsBloodOnTheAltar RottingRemains
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

rottingRemainsBloodOnTheAltar :: TreacheryCard RottingRemainsBloodOnTheAltar
rottingRemainsBloodOnTheAltar =
  treachery
    (RottingRemainsBloodOnTheAltar . RottingRemains)
    Cards.rottingRemainsBloodOnTheAltar

instance RunMessage RottingRemainsBloodOnTheAltar where
  runMessage msg (RottingRemainsBloodOnTheAltar inner) =
    RottingRemainsBloodOnTheAltar <$> runMessage msg inner
