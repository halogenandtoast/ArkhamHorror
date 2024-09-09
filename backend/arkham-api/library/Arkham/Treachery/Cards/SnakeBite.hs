module Arkham.Treachery.Cards.SnakeBite (snakeBite, SnakeBite (..)) where

import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Matcher
import Arkham.Message qualified as Msg
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
      let source = toSource attrs
      allies <- select $ #ally <> assetControlledBy iid
      chooseOrRunOneM iid do
        when (notNull allies) do
          labeled "Deal 5 damage to an Ally asset you control" do
            chooseTargetM iid allies \ally ->
              push $ Msg.DealAssetDamage ally source 5 0
        labeled
          "Take 1 direct damage. If you are not poisoned, put a set-aside Poisoned weakness into play in your threat area."
          do
            directDamage iid source 1
            unlessM (getIsPoisoned iid) do
              poisoned <- getSetAsidePoisoned
              push $ CreateWeaknessInThreatArea poisoned iid
      pure t
    _ -> SnakeBite <$> liftRunMessage msg attrs
