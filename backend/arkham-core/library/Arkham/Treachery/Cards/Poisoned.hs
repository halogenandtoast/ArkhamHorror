module Arkham.Treachery.Cards.Poisoned (
  poisoned,
  Poisoned (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Poisoned = Poisoned TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

poisoned :: TreacheryCard Poisoned
poisoned = treachery Poisoned Cards.poisoned

instance RunMessage Poisoned where
  runMessage msg t@(Poisoned attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Poisoned <$> runMessage msg attrs
