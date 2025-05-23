module Arkham.Asset.Assets.StrangeSolutionAcidicIchor4 (strangeSolutionAcidicIchor4) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestSource)
import Arkham.SkillType
import Arkham.Taboo

newtype StrangeSolutionAcidicIchor4 = StrangeSolutionAcidicIchor4 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionAcidicIchor4 :: AssetCard StrangeSolutionAcidicIchor4
strangeSolutionAcidicIchor4 = asset StrangeSolutionAcidicIchor4 Cards.strangeSolutionAcidicIchor4

instance HasAbilities StrangeSolutionAcidicIchor4 where
  getAbilities (StrangeSolutionAcidicIchor4 attrs) =
    [fightAbility attrs 1 (assetUseCost attrs Supply 1) ControlsThis]

instance HasModifiersFor StrangeSolutionAcidicIchor4 where
  getModifiersFor (StrangeSolutionAcidicIchor4 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      pure [BaseSkillOf SkillCombat 6, DamageDealt $ if tabooed TabooList20 a then 1 else 2]

instance RunMessage StrangeSolutionAcidicIchor4 where
  runMessage msg a@(StrangeSolutionAcidicIchor4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    _ -> StrangeSolutionAcidicIchor4 <$> liftRunMessage msg attrs
