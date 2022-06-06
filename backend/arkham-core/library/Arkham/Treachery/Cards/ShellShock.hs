module Arkham.Treachery.Cards.ShellShock
  ( shellShock
  , ShellShock(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Message hiding (InvestigatorDamage)
import Arkham.Projection
import Arkham.Treachery.Attrs
import Arkham.Treachery.Cards qualified as Cards

newtype ShellShock = ShellShock TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shellShock :: TreacheryCard ShellShock
shellShock = treachery ShellShock Cards.shellShock

instance RunMessage ShellShock where
  runMessage msg t@(ShellShock attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      horrorCount <- fieldMap InvestigatorDamage (`div` 2) iid
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 horrorCount)
    _ -> ShellShock <$> runMessage msg attrs
