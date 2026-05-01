module Arkham.Treachery.Cards.RedHerring (redHerring) where

import Arkham.Calculation
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype RedHerring = RedHerring TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

redHerring :: TreacheryCard RedHerring
redHerring = treachery RedHerring Cards.redHerring

instance RunMessage RedHerring where
  runMessage msg t@(RedHerring attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect
        $ IfInvestigatorExistsCalculation iid (InvestigatorWithClues $ atLeast 2) (Fixed 4) (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      placeCluesOnLocation iid attrs 1
      pure t
    _ -> RedHerring <$> liftRunMessage msg attrs
