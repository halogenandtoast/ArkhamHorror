module Arkham.Asset.Assets.TheBlackFan3 (theBlackFan3, TheBlackFan3 (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype TheBlackFan3 = TheBlackFan3 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBlackFan3 :: AssetCard TheBlackFan3
theBlackFan3 = asset TheBlackFan3 Cards.theBlackFan3

instance HasModifiersFor TheBlackFan3 where
  getModifiersFor (TheBlackFan3 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      resources <- field InvestigatorResources iid
      modifiedWhen_ a (resources >= 10) iid
        $ [HealthModifier 1, SanityModifier 1]
        <> (guard (resources >= 15) *> [AdditionalActions "The Black Fan" (toSource a) 1])
        <> (guard (resources >= 20) *> [SkillModifier skill 1 | skill <- [minBound ..]])

instance RunMessage TheBlackFan3 where
  runMessage msg (TheBlackFan3 attrs) = TheBlackFan3 <$> runMessage msg attrs
