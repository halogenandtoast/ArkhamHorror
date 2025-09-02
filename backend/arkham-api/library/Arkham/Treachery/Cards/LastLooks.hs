module Arkham.Treachery.Cards.LastLooks (lastLooks) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LastLooks = LastLooks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lastLooks :: TreacheryCard LastLooks
lastLooks = treachery LastLooks Cards.lastLooks

instance RunMessage LastLooks where
  runMessage msg t@(LastLooks attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower
        $ IfInvestigatorExistsCalculation iid (InvestigatorWithDamage (atLeast 3)) (Fixed 4) (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      directHorror iid attrs 1
      randomDiscard iid attrs
      pure t
    _ -> LastLooks <$> liftRunMessage msg attrs
