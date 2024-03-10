module Arkham.Treachery.Cards.FalseAwakening
  ( falseAwakening
  , FalseAwakening(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype FalseAwakening = FalseAwakening TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseAwakening :: TreacheryCard FalseAwakening
falseAwakening = treachery FalseAwakening Cards.falseAwakening

instance RunMessage FalseAwakening where
  runMessage msg t@(FalseAwakening attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> FalseAwakening <$> runMessage msg attrs
