module Arkham.Asset.Cards.StrangeSolutionAcidicIchor4 (
  strangeSolutionAcidicIchor4,
  StrangeSolutionAcidicIchor4 (..),
) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude
import Arkham.SkillType

newtype StrangeSolutionAcidicIchor4 = StrangeSolutionAcidicIchor4 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionAcidicIchor4 :: AssetCard StrangeSolutionAcidicIchor4
strangeSolutionAcidicIchor4 = asset StrangeSolutionAcidicIchor4 Cards.strangeSolutionAcidicIchor4

instance HasAbilities StrangeSolutionAcidicIchor4 where
  getAbilities (StrangeSolutionAcidicIchor4 attrs) =
    [fightAbility attrs 1 (assetUseCost attrs Supply 1) ControlsThis]

instance HasModifiersFor StrangeSolutionAcidicIchor4 where
  getModifiersFor (InvestigatorTarget iid) (StrangeSolutionAcidicIchor4 a) | controlledBy a iid = do
    toModifiers a . fromMaybe [] <$> runMaybeT do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      Action.Fight <- MaybeT getSkillTestAction
      pure [BaseSkillOf SkillCombat 6, DamageDealt 2]
  getModifiersFor _ _ = pure []

instance RunMessage StrangeSolutionAcidicIchor4 where
  runMessage msg a@(StrangeSolutionAcidicIchor4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkChooseFight iid (attrs.ability 1)
      pure a
    _ -> StrangeSolutionAcidicIchor4 <$> runMessage msg attrs
