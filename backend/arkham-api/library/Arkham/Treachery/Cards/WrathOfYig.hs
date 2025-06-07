module Arkham.Treachery.Cards.WrathOfYig (wrathOfYig) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WrathOfYig = WrathOfYig TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrathOfYig :: TreacheryCard WrathOfYig
wrathOfYig = treachery WrathOfYig Cards.wrathOfYig

instance RunMessage WrathOfYig where
  runMessage msg t@(WrathOfYig attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (SumCalculation [Fixed 2, VengeanceCalculation])
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      isPoisoned <- getIsPoisoned iid
      if isPoisoned
        then assignDamage iid attrs 2
        else do
          poisoned <- getSetAsidePoisoned
          createWeaknessInThreatArea poisoned iid
      pure t
    _ -> WrathOfYig <$> liftRunMessage msg attrs
