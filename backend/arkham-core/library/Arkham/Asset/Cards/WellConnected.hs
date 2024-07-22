module Arkham.Asset.Cards.WellConnected (wellConnected, wellConnectedEffect, WellConnected (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Import
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype WellConnected = WellConnected AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected :: AssetCard WellConnected
wellConnected = asset WellConnected Cards.wellConnected

instance HasAbilities WellConnected where
  getAbilities (WellConnected a) = [controlledAbility a 1 DuringAnySkillTest $ FastAbility $ exhaust a]

instance RunMessage WellConnected where
  runMessage msg a@(WellConnected attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ createCardEffect Cards.wellConnected Nothing (toSource attrs) sid
      pure a
    _ -> WellConnected <$> runMessage msg attrs

newtype WellConnectedEffect = WellConnectedEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnectedEffect :: EffectArgs -> WellConnectedEffect
wellConnectedEffect = cardEffect WellConnectedEffect Cards.wellConnected

instance HasModifiersFor WellConnectedEffect where
  getModifiersFor (InvestigatorTarget iid) (WellConnectedEffect a) = maybeModified a do
    sid <- MaybeT getSkillTestId
    iid' <- MaybeT getSkillTestInvestigator
    guard $ isTarget sid a.target
    guard $ iid == iid'
    resources <- lift $ field InvestigatorResources iid
    pure [AnySkillValue (resources `div` 5)]
  getModifiersFor _ _ = pure []

instance RunMessage WellConnectedEffect where
  runMessage msg e@(WellConnectedEffect attrs) = case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push (DisableEffect attrs.id)
      pure e
    _ -> WellConnectedEffect <$> runMessage msg attrs
