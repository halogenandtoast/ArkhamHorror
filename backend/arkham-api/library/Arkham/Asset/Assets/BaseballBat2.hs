module Arkham.Asset.Assets.BaseballBat2 (baseballBat2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.DamageEffect (attack)
import Arkham.Helpers.ChaosToken (getModifiedChaosTokenFaces)
import Arkham.Helpers.SkillTest (
  getSkillTestResultWithResultModifiers,
  getSkillTestRevealedChaosTokens,
  getSkillTestSource,
  getSkillTestTargetedEnemy,
 )
import Arkham.I18n
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillTestResult

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
    SkillTestEnds _ iid _ -> do
      whenJustM getSkillTestSource \source ->
        when (isAbilitySource attrs 1 source) do
          tokens <- getModifiedChaosTokenFaces =<< getSkillTestRevealedChaosTokens
          when (any (`elem` [Skull, AutoFail]) tokens) do
            -- The choice is ST.8, so ST.7 has already dealt the attack's damage
            -- and a DamageDealt modifier would have nothing left to modify. The
            -- extra point is applied retroactively instead. Resolve the target
            -- here, while the test is still around; the attack is over, so the
            -- enemy's state is already final.
            mTarget <- runMaybeT do
              SucceededBy _ _ <- MaybeT getSkillTestResultWithResultModifiers
              eid <- MaybeT getSkillTestTargetedEnemy
              liftGuardM $ selectAny (Matcher.EnemyWithId eid)
              pure eid
            afterSkillTest iid "Baseball Bat (2)" do
              chooseOneM iid do
                (cardI18n $ labeled "baseballBat2.returnToHand") $ returnToHand iid attrs
                (cardI18n $ labeled "baseballBat2.extraDamage") do
                  for_ mTarget \eid -> push $ DealDamage (EnemyTarget eid) (attack (attrs.ability 1) 1)
                  toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> BaseballBat2 <$> liftRunMessage msg attrs
