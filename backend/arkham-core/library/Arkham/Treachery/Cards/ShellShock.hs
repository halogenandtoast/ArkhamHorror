module Arkham.Treachery.Cards.ShellShock (
  shellShock,
  ShellShock (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ShellShock = ShellShock TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

shellShock :: TreacheryCard ShellShock
shellShock = treachery ShellShock Cards.shellShock

instance RunMessage ShellShock where
  runMessage msg t@(ShellShock attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horrorCount <- fieldMap InvestigatorDamage (`div` 2) iid
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 horrorCount)
    _ -> ShellShock <$> runMessage msg attrs
