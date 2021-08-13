module Arkham.Types.Asset.Cards.Encyclopedia2
  ( Encyclopedia2(..)
  , encyclopedia2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Encyclopedia2 = Encyclopedia2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia2 :: AssetCard Encyclopedia2
encyclopedia2 = hand Encyclopedia2 Cards.encyclopedia2

instance HasActions Encyclopedia2 where
  getActions (Encyclopedia2 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ Costs
        [ActionCost 1, ExhaustThis]
    ]

instance (AssetRunner env) => RunMessage env Encyclopedia2 where
  runMessage msg (Encyclopedia2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList locationId
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
        | target <- investigatorTargets
        ]
      pure $ Encyclopedia2 $ attrs & exhaustedL .~ True
    _ -> Encyclopedia2 <$> runMessage msg attrs
