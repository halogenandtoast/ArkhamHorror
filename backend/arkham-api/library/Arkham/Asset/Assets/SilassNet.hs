module Arkham.Asset.Assets.SilassNet (silassNet) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype SilassNet = SilassNet AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

silassNet :: AssetCard SilassNet
silassNet = asset SilassNet Cards.silassNet

instance HasAbilities SilassNet where
  getAbilities (SilassNet attrs) = [restricted attrs 1 ControlsThis evadeAction_]

instance RunMessage SilassNet where
  runMessage msg a@(SilassNet attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 1)
      chooseEvadeEnemy sid iid (attrs.ability 1)
      pure a
    SkillTestEnds _ iid (isAbilitySource attrs 1 -> True) -> do
      miid <- getSkillTestInvestigator
      when (Just iid == miid) do
        skills <- select $ skillOwnedBy iid
        chooseOneM iid do
          cardI18n $ scope "silassNet" $ labeled' "returnNetWithSkills" do
            returnToHand iid attrs
            for_ skills (returnToHand iid)
          labeledI "doNothing" nothing
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTargetedEnemy >>= traverse_ \eid -> do
        otherEnemies <- select $ enemyEngagedWith iid <> not_ (EnemyWithId eid)
        when (notNull otherEnemies) do
          chooseOneM iid do
            labeledI "automaticallyEvadeAnotherEnemy" do
              chooseOrRunOneM iid $ targets otherEnemies (automaticallyEvadeEnemy iid)
            labeledI "doNotEvadeAnotherEnemy" nothing
      pure a
    _ -> SilassNet <$> liftRunMessage msg attrs
