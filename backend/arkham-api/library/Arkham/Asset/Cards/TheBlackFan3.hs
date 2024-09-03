module Arkham.Asset.Cards.TheBlackFan3 (
  theBlackFan3,
  TheBlackFan3 (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

newtype TheBlackFan3 = TheBlackFan3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackFan3 :: AssetCard TheBlackFan3
theBlackFan3 = asset TheBlackFan3 Cards.theBlackFan3

instance HasModifiersFor TheBlackFan3 where
  getModifiersFor (InvestigatorTarget iid) (TheBlackFan3 a) | a `controlledBy` iid = do
    resources <- field InvestigatorResources iid
    pure
      $ toModifiers a
      $ (guard (resources >= 10) *> [HealthModifier 1, SanityModifier 1])
      <> (guard (resources >= 15) *> [AdditionalActions "The Black Fan" (toSource a) 1])
      <> (guard (resources >= 20) *> [SkillModifier skill 1 | skill <- [minBound ..]])
  getModifiersFor _ _ = pure []

instance RunMessage TheBlackFan3 where
  runMessage msg (TheBlackFan3 attrs) = TheBlackFan3 <$> runMessage msg attrs
