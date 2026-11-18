module Arkham.Treachery.Cards.ShellShock (shellShock) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.CardDefs.ThePathToCarcosa qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype ShellShock = ShellShock TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shellShock :: TreacheryCard ShellShock
shellShock = treachery ShellShock Cards.shellShock

instance RunMessage ShellShock where
  runMessage msg t@(ShellShock attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      -- Damage assignment is deferred, so a point already assigned to the investigator
      -- is still pending here; Mark's own reaction can reveal this mid-assignment (#6011)
      damage <- field InvestigatorDamage iid
      pending <- field InvestigatorAssignedDamage iid
      assignHorror iid attrs ((damage + pending) `div` 2)
      pure t
    _ -> ShellShock <$> liftRunMessage msg attrs
