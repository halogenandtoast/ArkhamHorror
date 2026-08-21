module Arkham.Treachery.Cards.ChildrenOfBlood.Afflicted.RottingRemains (rottingRemains) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.Afflicted qualified as Cards
import Arkham.Treachery.Cards.NightOfTheZealot.StrikingFear.RottingRemains qualified as Base
import Arkham.Treachery.Import.Lifted

newtype RottingRemains = RottingRemains Base.RottingRemains
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

rottingRemains :: TreacheryCard RottingRemains
rottingRemains = treachery (RottingRemains . Base.RottingRemains) Cards.rottingRemains

instance RunMessage RottingRemains where
  runMessage msg (RottingRemains inner) = RottingRemains <$> runMessage msg inner
