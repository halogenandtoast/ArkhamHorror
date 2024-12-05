module Arkham.Asset.Assets.TwentyFiveAutomatic (twentyFiveAutomatic, TwentyFiveAutomatic (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTarget)
import Arkham.Matcher

newtype TwentyFiveAutomatic = TwentyFiveAutomatic AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

twentyFiveAutomatic :: AssetCard TwentyFiveAutomatic
twentyFiveAutomatic = asset TwentyFiveAutomatic Cards.twentyFiveAutomatic

instance HasAbilities TwentyFiveAutomatic where
  getAbilities (TwentyFiveAutomatic attrs) = [restrictedAbility attrs 1 ControlsThis $ fightAction $ assetUseCost attrs Ammo 1]

instance HasModifiersFor TwentyFiveAutomatic where
  getModifiersFor (TwentyFiveAutomatic a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      EnemyTarget eid' <- MaybeT getSkillTestTarget
      liftGuardM $ eid' <=~> ExhaustedEnemy
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource a 1 source
      pure [SkillModifier #combat 2, DamageDealt 1]

instance RunMessage TwentyFiveAutomatic where
  runMessage msg a@(TwentyFiveAutomatic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> TwentyFiveAutomatic <$> liftRunMessage msg attrs
