{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Asset.Cards.Encyclopedia2
  ( Encyclopedia2(..)
  , encyclopedia2
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype Encyclopedia2 = Encyclopedia2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

encyclopedia2 :: AssetId -> Encyclopedia2
encyclopedia2 uuid =
  Encyclopedia2 $ (baseAttrs uuid "01042") { assetSlots = [HandSlot] }

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
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- getId @LocationId iid
      investigatorTargets <- map InvestigatorTarget <$> getSetList locationId
      unshiftMessage $ chooseOne
        iid
        [ TargetLabel
            target
            [ chooseOne
                iid
                [ Label
                  "Willpower"
                  [ CreatePhaseEffect
                      (EffectModifiers
                      $ toModifiers attrs [SkillModifier SkillWillpower 2]
                      )
                      source
                      target
                  ]
                , Label
                  "Intellect"
                  [ CreatePhaseEffect
                      (EffectModifiers
                      $ toModifiers attrs [SkillModifier SkillIntellect 2]
                      )
                      source
                      target
                  ]
                , Label
                  "Combat"
                  [ CreatePhaseEffect
                      (EffectModifiers
                      $ toModifiers attrs [SkillModifier SkillCombat 2]
                      )
                      source
                      target
                  ]
                , Label
                  "Agility"
                  [ CreatePhaseEffect
                      (EffectModifiers
                      $ toModifiers attrs [SkillModifier SkillAgility 2]
                      )
                      source
                      target
                  ]
                ]
            ]
        | target <- investigatorTargets
        ]
      pure $ Encyclopedia2 $ attrs & exhaustedL .~ True
    _ -> Encyclopedia2 <$> runMessage msg attrs
