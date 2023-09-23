module Arkham.Asset.Cards.Encyclopedia (
  Encyclopedia (..),
  encyclopedia,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Encyclopedia = Encyclopedia AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia :: AssetCard Encyclopedia
encyclopedia = asset Encyclopedia Cards.encyclopedia

instance HasAbilities Encyclopedia where
  getAbilities (Encyclopedia a) =
    [ restrictedAbility a 1 ControlsThis $ actionAbilityWithCost $ exhaust a <> assetUseCost a Secret 1
    ]

instance RunMessage Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      targets <- selectTargets $ colocatedWith iid
      push
        $ chooseOne iid
        $ targetLabels targets
        $ \target ->
          only
            $ chooseOne iid
            $ [ Label label [phaseModifier source target (SkillModifier skill 2)]
              | (label, skill) <- labeledSkills
              ]
      pure a
    _ -> Encyclopedia <$> runMessage msg attrs
