module Arkham.Types.Asset.Cards.Encyclopedia2
  ( Encyclopedia2(..)
  , encyclopedia2
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
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
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Encyclopedia2 = Encyclopedia2 AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia2 :: AssetCard Encyclopedia2
encyclopedia2 = hand Encyclopedia2 Cards.encyclopedia2

instance HasModifiersFor env Encyclopedia2 where
  getModifiersFor = noModifiersFor

instance HasActions env Encyclopedia2 where
  getActions iid NonFast (Encyclopedia2 a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs [ActionCost 1, ExhaustCost (toTarget a)]
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Encyclopedia2 where
  runMessage msg (Encyclopedia2 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList locationId
      unshiftMessage $ chooseOne
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
