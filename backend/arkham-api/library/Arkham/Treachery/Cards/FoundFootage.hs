module Arkham.Treachery.Cards.FoundFootage (foundFootage) where

import Arkham.Location.Types (Field (..))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FoundFootage = FoundFootage TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

foundFootage :: TreacheryCard FoundFootage
foundFootage = treachery FoundFootage Cards.foundFootage

instance RunMessage FoundFootage where
  runMessage msg t@(FoundFootage attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ InvestigatorLocationMaybeFieldCalculation iid LocationShroud
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> FoundFootage <$> liftRunMessage msg attrs
