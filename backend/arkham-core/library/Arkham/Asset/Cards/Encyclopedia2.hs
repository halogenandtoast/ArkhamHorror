module Arkham.Asset.Cards.Encyclopedia2 (
  Encyclopedia2 (..),
  encyclopedia2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Encyclopedia2 = Encyclopedia2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia2 :: AssetCard Encyclopedia2
encyclopedia2 = asset Encyclopedia2 Cards.encyclopedia2

instance HasAbilities Encyclopedia2 where
  getAbilities (Encyclopedia2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ Costs [ActionCost 1, exhaust a]
    ]

instance RunMessage Encyclopedia2 where
  runMessage msg a@(Encyclopedia2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      targets <- selectTargets $ colocatedWith iid
      push
        $ chooseOne iid
        $ [ targetLabel target
            $ [ chooseOne iid
                  $ [ Label label [phaseModifier (toAbilitySource attrs 1) target $ SkillModifier skill 2]
                    | (label, skill) <-
                        [ ("Willpower", #willpower)
                        , ("Intellect", #intellect)
                        , ("Combat", #combat)
                        , ("Agility", #agility)
                        ]
                    ]
              ]
          | target <- targets
          ]
      pure a
    _ -> Encyclopedia2 <$> runMessage msg attrs
