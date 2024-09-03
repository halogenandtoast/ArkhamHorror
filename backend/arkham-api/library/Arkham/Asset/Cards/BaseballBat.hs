module Arkham.Asset.Cards.BaseballBat (BaseballBat (..), baseballBat) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest (getSkillTestRevealedChaosTokens, getSkillTestSource)
import Arkham.Modifier

newtype BaseballBat = BaseballBat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

baseballBat :: AssetCard BaseballBat
baseballBat = asset BaseballBat Cards.baseballBat

instance HasAbilities BaseballBat where
  getAbilities (BaseballBat a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage BaseballBat where
  runMessage msg a@(BaseballBat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    SkillTestEnds _ iid _ -> do
      whenJustM getSkillTestSource \source ->
        if isAbilitySource attrs 1 source
          then do
            tokens <- map (.face) <$> getSkillTestRevealedChaosTokens
            when (any (`elem` [Skull, AutoFail]) tokens) do
              afterSkillTest $ toDiscardBy iid (attrs.ability 1) attrs
          else pure ()
      pure a
    _ -> BaseballBat <$> liftRunMessage msg attrs
