module Arkham.Treachery.Cards.DeathApproaches (
  deathApproaches,
  DeathApproaches (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeathApproaches = DeathApproaches TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deathApproaches :: TreacheryCard DeathApproaches
deathApproaches = treachery DeathApproaches Cards.deathApproaches

instance RunMessage DeathApproaches where
  runMessage msg t@(DeathApproaches attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> DeathApproaches <$> runMessage msg attrs
