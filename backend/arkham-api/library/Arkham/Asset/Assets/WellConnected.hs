module Arkham.Asset.Assets.WellConnected (wellConnected, wellConnectedEffect, WellConnected (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Import
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype WellConnected = WellConnected AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected :: AssetCard WellConnected
wellConnected = asset WellConnected Cards.wellConnected

instance HasAbilities WellConnected where
  getAbilities (WellConnected a) =
    [ wantsSkillTest (YourSkillTest #any)
        $ controlledAbility a 1 (DuringSkillTest #any)
        $ FastAbility (exhaust a)
    ]

instance RunMessage WellConnected where
  runMessage msg a@(WellConnected attrs) = case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push =<< createCardEffect Cards.wellConnected Nothing (toSource attrs) sid
      pure a
    _ -> WellConnected <$> runMessage msg attrs

newtype WellConnectedEffect = WellConnectedEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnectedEffect :: EffectArgs -> WellConnectedEffect
wellConnectedEffect = cardEffect WellConnectedEffect Cards.wellConnected

instance HasModifiersFor WellConnectedEffect where
  getModifiersFor (WellConnectedEffect a) =
    getSkillTest >>= \case
      Nothing -> pure mempty
      Just st -> maybeModified_ a a.target do
        guard $ isTarget st.investigator a.target
        resources <- lift $ field InvestigatorResources st.investigator
        pure [AnySkillValue (resources `div` 5)]

instance RunMessage WellConnectedEffect where
  runMessage msg e@(WellConnectedEffect attrs) = case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push (DisableEffect attrs.id)
      pure e
    _ -> WellConnectedEffect <$> runMessage msg attrs
