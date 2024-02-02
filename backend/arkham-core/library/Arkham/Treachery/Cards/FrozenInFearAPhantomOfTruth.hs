module Arkham.Treachery.Cards.FrozenInFearAPhantomOfTruth (
  frozenInFearAPhantomOfTruth,
  FrozenInFearAPhantomOfTruth (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Cards.FrozenInFear
import Arkham.Treachery.Runner

newtype FrozenInFearAPhantomOfTruth = FrozenInFearAPhantomOfTruth FrozenInFear
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasModifiersFor, HasAbilities)

frozenInFearAPhantomOfTruth :: TreacheryCard FrozenInFearAPhantomOfTruth
frozenInFearAPhantomOfTruth =
  treachery (FrozenInFearAPhantomOfTruth . FrozenInFear) Cards.frozenInFearAPhantomOfTruth

instance RunMessage FrozenInFearAPhantomOfTruth where
  runMessage msg (FrozenInFearAPhantomOfTruth attrs) =
    FrozenInFearAPhantomOfTruth <$> runMessage msg attrs
