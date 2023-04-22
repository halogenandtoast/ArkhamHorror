module Arkham.Asset.Cards.QuickStudy2
  ( quickStudy2
  , QuickStudy2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype QuickStudy2 = QuickStudy2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quickStudy2 :: AssetCard QuickStudy2
quickStudy2 = asset QuickStudy2 Cards.quickStudy2

instance HasAbilities QuickStudy2 where
  getAbilities (QuickStudy2 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ ExhaustCost (toTarget a)
        <> PlaceClueOnLocationCost 1
    ]

instance RunMessage QuickStudy2 where
  runMessage msg a@(QuickStudy2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs (InvestigatorTarget iid) $ AnySkillValue 3
      pure a
    _ -> QuickStudy2 <$> runMessage msg attrs
