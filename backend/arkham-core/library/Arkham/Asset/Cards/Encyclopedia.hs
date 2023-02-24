module Arkham.Asset.Cards.Encyclopedia
  ( Encyclopedia(..)
  , encyclopedia
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.SkillType

newtype Encyclopedia = Encyclopedia AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia :: AssetCard Encyclopedia
encyclopedia = asset Encyclopedia Cards.encyclopedia

instance HasAbilities Encyclopedia where
  getAbilities (Encyclopedia a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ Costs
        [ ActionCost 1
        , ExhaustCost (toTarget a)
        , UseCost (AssetWithId $ toId a) Secret 1
        ]
    ]

instance RunMessage Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      targets <- selectListMap InvestigatorTarget $ colocatedWith iid
      push $ chooseOne
        iid
        [ TargetLabel
            target
            [ chooseOne
                iid
                [ Label
                    label
                    [ CreateWindowModifierEffect
                        EffectPhaseWindow
                        (EffectModifiers
                        $ toModifiers attrs [SkillModifier skill 2]
                        )
                        source
                        target
                    ]
                | (label, skill) <-
                  [ ("Willpower", SkillWillpower)
                  , ("Intellect", SkillIntellect)
                  , ("Combat", SkillCombat)
                  , ("Agility", SkillAgility)
                  ]
                ]
            ]
        | target <- targets
        ]
      pure a
    _ -> Encyclopedia <$> runMessage msg attrs
