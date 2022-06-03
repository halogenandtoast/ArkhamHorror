module Arkham.Treachery.Cards.FrozenInFearAPhantomOfTruth
  ( frozenInFearAPhantomOfTruth
  , FrozenInFearAPhantomOfTruth(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner
import Arkham.Treachery.Cards.FrozenInFear

newtype FrozenInFearAPhantomOfTruth = FrozenInFearAPhantomOfTruth FrozenInFear
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor env, HasAbilities)

frozenInFearAPhantomOfTruth :: TreacheryCard FrozenInFearAPhantomOfTruth
frozenInFearAPhantomOfTruth =
  treachery (FrozenInFearAPhantomOfTruth . FrozenInFear) Cards.frozenInFearAPhantomOfTruth

instance TreacheryRunner env => RunMessage FrozenInFearAPhantomOfTruth where
  runMessage msg (FrozenInFearAPhantomOfTruth attrs) =
    FrozenInFearAPhantomOfTruth <$> runMessage msg attrs
