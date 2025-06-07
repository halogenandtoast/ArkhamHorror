module Arkham.Treachery.Cards.SnakeBite (snakeBite) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SnakeBite = SnakeBite TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snakeBite :: TreacheryCard SnakeBite
snakeBite = treachery SnakeBite Cards.snakeBite

instance RunMessage SnakeBite where
  runMessage msg t@(SnakeBite attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility $ Fixed 3
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> assetControlledBy iid
      chooseOrRunOneM iid $ campaignI18n do
        when (notNull allies) do
          labeled' "snakeBite.damageAlly" $ chooseTargetM iid allies \ally -> dealAssetDamage ally attrs 5
        labeled' "snakeBite.damageSelf" do
          directDamage iid attrs 1
          unlessM (getIsPoisoned iid) do
            poisoned <- getSetAsidePoisoned
            createWeaknessInThreatArea poisoned iid
      pure t
    _ -> SnakeBite <$> liftRunMessage msg attrs
