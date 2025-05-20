module Arkham.Treachery.Cards.LostSoul (lostSoul) where

import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LostSoul = LostSoul TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostSoul :: TreacheryCard LostSoul
lostSoul = treachery LostSoul Cards.lostSoul

instance RunMessage LostSoul where
  runMessage msg t@(LostSoul attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      moreConvictionThanDoubt <- getMoreConvictionThanDoubt
      sid <- getRandom
      if moreConvictionThanDoubt
        then revelationSkillTest sid iid attrs #willpower $ InvestigatorFieldCalculation iid #intellect
        else revelationSkillTest sid iid attrs #intellect $ InvestigatorFieldCalculation iid #willpower
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 2
      pure t
    _ -> LostSoul <$> liftRunMessage msg attrs
