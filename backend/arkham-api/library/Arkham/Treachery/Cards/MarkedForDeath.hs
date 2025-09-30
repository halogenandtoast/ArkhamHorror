module Arkham.Treachery.Cards.MarkedForDeath (markedForDeath) where

import Arkham.Investigator.Types (Field (..))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MarkedForDeath = MarkedForDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForDeath :: TreacheryCard MarkedForDeath
markedForDeath = treachery MarkedForDeath Cards.markedForDeath

instance RunMessage MarkedForDeath where
  runMessage msg t@(MarkedForDeath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility
        $ SumCalculation [Fixed 2, InvestigatorFieldCalculation iid InvestigatorHorror]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> MarkedForDeath <$> liftRunMessage msg attrs
