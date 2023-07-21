module Arkham.Asset.Cards.WellConnected3 (
  wellConnected3,
  wellConnected3Effect,
  WellConnected3 (..),
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

newtype WellConnected3 = WellConnected3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected3 :: AssetCard WellConnected3
wellConnected3 =
  asset WellConnected3 Cards.wellConnected3

instance HasAbilities WellConnected3 where
  getAbilities (WellConnected3 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest) $
        FastAbility $
          ExhaustCost $
            toTarget a
    , limitedAbility (PlayerLimit PerRound 1) $
        restrictedAbility a 2 ControlsThis $
          FastAbility $
            ResourceCost 2
    ]

instance RunMessage WellConnected3 where
  runMessage msg a@(WellConnected3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ createCardEffect Cards.wellConnected3 Nothing (toSource attrs) (InvestigatorTarget iid)
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ Ready (toTarget attrs)
      pure a
    _ -> WellConnected3 <$> runMessage msg attrs

newtype WellConnected3Effect = WellConnected3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wellConnected3Effect :: EffectArgs -> WellConnected3Effect
wellConnected3Effect = cardEffect WellConnected3Effect Cards.wellConnected3

instance HasModifiersFor WellConnected3Effect where
  getModifiersFor target@(InvestigatorTarget iid) (WellConnected3Effect a) | effectTarget a == target = do
    resources <- field InvestigatorResources iid
    pure $ toModifiers a [AnySkillValue (resources `div` 4)]
  getModifiersFor _ _ = pure []

instance RunMessage WellConnected3Effect where
  runMessage msg e@(WellConnected3Effect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ -> do
      push (DisableEffect effectId)
      pure e
    _ -> WellConnected3Effect <$> runMessage msg attrs
