module Arkham.Treachery.Cards.ThePathToCarcosa.APhantomOfTruth.FrozenInFear (frozenInFear) where

import Arkham.Treachery.CardDefs.ThePathToCarcosa.APhantomOfTruth qualified as Cards
import Arkham.Treachery.Cards.NightOfTheZealot.StrikingFear.FrozenInFear qualified as Base
import Arkham.Treachery.Import.Lifted

newtype FrozenInFear = FrozenInFear Base.FrozenInFear
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor, HasAbilities)

frozenInFear :: TreacheryCard FrozenInFear
frozenInFear =
  treachery
    (FrozenInFear . Base.FrozenInFear)
    Cards.frozenInFear

instance RunMessage FrozenInFear where
  runMessage msg (FrozenInFear attrs) = FrozenInFear <$> runMessage msg attrs
