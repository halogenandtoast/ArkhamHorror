module Arkham.Asset.Cards.WellConnected (
  wellConnected,
  wellConnectedEffect,
  WellConnected (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype WellConnected = WellConnected AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected :: AssetCard WellConnected
wellConnected =
  asset WellConnected Cards.wellConnected

instance HasAbilities WellConnected where
  getAbilities (WellConnected a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest) $
        FastAbility $
          ExhaustCost $
            toTarget a
    ]

instance RunMessage WellConnected where
  runMessage msg a@(WellConnected attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ createCardEffect Cards.wellConnected Nothing (toSource attrs) (InvestigatorTarget iid)
      pure a
    _ -> WellConnected <$> runMessage msg attrs

newtype WellConnectedEffect = WellConnectedEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnectedEffect :: EffectArgs -> WellConnectedEffect
wellConnectedEffect = cardEffect WellConnectedEffect Cards.wellConnected

instance HasModifiersFor WellConnectedEffect where
  getModifiersFor target@(InvestigatorTarget iid) (WellConnectedEffect a) | effectTarget a == target = do
    resources <- field InvestigatorResources iid
    pure $ toModifiers a [AnySkillValue (resources `div` 5)]
  getModifiersFor _ _ = pure []

instance RunMessage WellConnectedEffect where
  runMessage msg e@(WellConnectedEffect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> WellConnectedEffect <$> runMessage msg attrs
