{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Encyclopedia where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses (Uses(..), useCount)
import qualified Arkham.Types.Asset.Uses as Resource

newtype Encyclopedia = Encyclopedia Attrs
  deriving newtype (Show, ToJSON, FromJSON)

encyclopedia :: AssetId -> Encyclopedia
encyclopedia uuid =
  Encyclopedia $ (baseAttrs uuid "60208") { assetSlots = [HandSlot] }

instance HasModifiersFor env Encyclopedia where
  getModifiersFor = noModifiersFor

instance HasActions env Encyclopedia where
  getActions iid NonFast (Encyclopedia a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | not (assetExhausted a) && useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Encyclopedia where
  runMessage msg (Encyclopedia attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Encyclopedia <$> runMessage msg (attrs & uses .~ Uses Resource.Secret 5)
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
                      (EffectModifiers [SkillModifier SkillWillpower 2])
                      source
                      target
                  ]
                , Label
                  "Intellect"
                  [ CreatePhaseEffect
                      (EffectModifiers [SkillModifier SkillIntellect 2])
                      source
                      target
                  ]
                , Label
                  "Combat"
                  [ CreatePhaseEffect
                      (EffectModifiers [SkillModifier SkillCombat 2])
                      source
                      target
                  ]
                , Label
                  "Agility"
                  [ CreatePhaseEffect
                      (EffectModifiers [SkillModifier SkillAgility 2])
                      source
                      target
                  ]
                ]
            ]
        | target <- investigatorTargets
        ]
      pure $ Encyclopedia $ attrs & exhausted .~ True & uses %~ Resource.use
    _ -> Encyclopedia <$> runMessage msg attrs
