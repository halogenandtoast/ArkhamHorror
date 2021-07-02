module Arkham.Types.Asset.Cards.Encyclopedia
  ( Encyclopedia(..)
  , encyclopedia
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target
import Arkham.Types.Window

newtype Encyclopedia = Encyclopedia AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encyclopedia :: AssetCard Encyclopedia
encyclopedia = hand Encyclopedia Cards.encyclopedia

instance HasModifiersFor env Encyclopedia where
  getModifiersFor = noModifiersFor

instance HasActions env Encyclopedia where
  getActions iid NonFast (Encyclopedia a) | ownedBy a iid = pure
    [ assetAction iid a 1 Nothing
        $ Costs
            [ActionCost 1, ExhaustCost (toTarget a), UseCost (toId a) Secret 1]
    ]
  getActions _ _ _ = pure []

instance AssetRunner env => RunMessage env Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Encyclopedia <$> runMessage msg (attrs & usesL .~ Uses Secret 5)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList locationId
      a <$ unshiftMessage
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
