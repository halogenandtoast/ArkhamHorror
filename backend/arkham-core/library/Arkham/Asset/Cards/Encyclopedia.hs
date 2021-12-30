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
import Arkham.Id
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype Encyclopedia = Encyclopedia AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia :: AssetCard Encyclopedia
encyclopedia = asset Encyclopedia Cards.encyclopedia

instance HasAbilities Encyclopedia where
  getAbilities (Encyclopedia a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustCost (toTarget a), UseCost (toId a) Secret 1]
    ]

instance AssetRunner env => RunMessage env Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList locationId
      a <$ push
        (chooseOne
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
          | target <- investigatorTargets
          ]
        )
    _ -> Encyclopedia <$> runMessage msg attrs
