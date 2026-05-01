module Arkham.Treachery.Cards.EagerForDeath2 (eagerForDeath2) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (InvestigatorDamage)

newtype EagerForDeath2 = EagerForDeath2 TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eagerForDeath2 :: TreacheryCard EagerForDeath2
eagerForDeath2 = treachery EagerForDeath2 Cards.eagerForDeath2

instance RunMessage EagerForDeath2 where
  runMessage msg t@(EagerForDeath2 attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ SumCalculation [Fixed 2, InvestigatorFieldCalculation iid InvestigatorDamage]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> EagerForDeath2 <$> liftRunMessage msg attrs
