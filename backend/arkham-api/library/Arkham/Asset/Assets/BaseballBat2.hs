module Arkham.Asset.Assets.BaseballBat2 (BaseballBat2 (..), baseballBat2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, getSkillTestSource)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype BaseballBat2 = BaseballBat2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat2 :: AssetCard BaseballBat2
baseballBat2 = asset BaseballBat2 Cards.baseballBat2

instance HasAbilities BaseballBat2 where
  getAbilities (BaseballBat2 a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat2 where
  runMessage msg a@(BaseballBat2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    SkillTestEnds sid iid _ -> do
      whenJustM getSkillTestSource \source ->
        when (isAbilitySource attrs 1 source) do
          tokens <- map (.face) <$> getSkillTestRevealedChaosTokens
          when (any (`elem` [Skull, AutoFail]) tokens) do
            afterSkillTest do
              chooseOneM iid do
                labeled "Return baseball Bat to your hand after this attack" $ returnToHand iid attrs
                labeled "This attack deals an additional +1 damage. Discard Baseball Bat after this attack" do
                  skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
                  toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> BaseballBat2 <$> liftRunMessage msg attrs
