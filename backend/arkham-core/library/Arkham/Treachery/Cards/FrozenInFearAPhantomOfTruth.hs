module Arkham.Treachery.Cards.FrozenInFearAPhantomOfTruth
  ( frozenInFearAPhantomOfTruth
  , FrozenInFearAPhantomOfTruth(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype FrozenInFearAPhantomOfTruth = FrozenInFearAPhantomOfTruth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenInFearAPhantomOfTruth :: TreacheryCard FrozenInFearAPhantomOfTruth
frozenInFearAPhantomOfTruth =
  treachery FrozenInFearAPhantomOfTruth Cards.frozenInFearAPhantomOfTruth

instance TreacheryRunner env => RunMessage env FrozenInFearAPhantomOfTruth where
  runMessage msg t@(FrozenInFearAPhantomOfTruth attrs) = case msg of
    Revelation _iid source | isSource attrs source ->
      t <$ push (Discard $ toTarget attrs)
    _ -> FrozenInFearAPhantomOfTruth <$> runMessage msg attrs
