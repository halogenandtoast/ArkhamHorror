module Arkham.Treachery.Cards.ShellShock (shellShock) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype ShellShock = ShellShock TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shellShock :: TreacheryCard ShellShock
shellShock = treachery ShellShock Cards.shellShock

instance RunMessage ShellShock where
  runMessage msg t@(ShellShock attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horrorCount <- fieldMap InvestigatorDamage (`div` 2) iid
      assignHorror iid attrs horrorCount
      pure t
    _ -> ShellShock <$> liftRunMessage msg attrs
