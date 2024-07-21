module Arkham.Asset.Cards.TwentyFiveAutomatic (twentyFiveAutomatic, TwentyFiveAutomatic (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
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
  getModifiersFor (InvestigatorTarget iid) (TwentyFiveAutomatic attrs) = do
    maybeModified attrs do
      guard $ attrs `controlledBy` iid
      EnemyTarget eid' <- MaybeT getSkillTestTarget
      liftGuardM $ eid' <=~> ExhaustedEnemy
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource attrs 1 source
      pure [SkillModifier #combat 2, DamageDealt 1]
  getModifiersFor _ _ = pure []

instance RunMessage TwentyFiveAutomatic where
  runMessage msg a@(TwentyFiveAutomatic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    _ -> TwentyFiveAutomatic <$> liftRunMessage msg attrs
