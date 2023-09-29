module Arkham.Asset.Cards.RiteOfSeeking4 (
  riteOfSeeking4,
  RiteOfSeeking4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate

newtype RiteOfSeeking4 = RiteOfSeeking4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: AssetCard RiteOfSeeking4
riteOfSeeking4 = asset RiteOfSeeking4 Cards.riteOfSeeking4

instance HasAbilities RiteOfSeeking4 where
  getAbilities (RiteOfSeeking4 a) = [investigateAbility a 1 (assetUseCost a Charge 1) ControlsThis]

instance RunMessage RiteOfSeeking4 where
  runMessage msg a@(RiteOfSeeking4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      lid <- getJustLocation iid
      investigation <-
        aspect iid source (#willpower `InsteadOf` #intellect) (mkInvestigate iid source)

      -- same effect as base
      pushAll
        $ [ createCardEffect Cards.riteOfSeeking Nothing source (InvestigationTarget iid lid)
          , skillTestModifier source iid (SkillModifier #willpower 2)
          ]
        <> leftOr investigation
      pure a
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
