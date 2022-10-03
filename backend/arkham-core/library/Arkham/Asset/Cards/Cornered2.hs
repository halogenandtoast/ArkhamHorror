module Arkham.Asset.Cards.Cornered2
  ( cornered2
  , Cornered2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.SkillTest
import Arkham.Target

newtype Cornered2 = Cornered2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cornered2 :: AssetCard Cornered2
cornered2 = asset Cornered2 Cards.cornered2

instance HasAbilities Cornered2 where
  getAbilities (Cornered2 a) =
    [ limitedAbility (PlayerLimit PerTestOrAbility 1)
        $ restrictedAbility a 1 (ControlsThis <> DuringSkillTest AnySkillTest)
        $ FastAbility
        $ HandDiscardCost 1 AnyCard
    ]

instance RunMessage Cornered2 where
  runMessage msg a@(Cornered2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      skillType <- skillTestSkillType <$> getJustSkillTest
      push $ skillTestModifier attrs (InvestigatorTarget iid) $ SkillModifier
        skillType
        2
      pure a
    _ -> Cornered2 <$> runMessage msg attrs
