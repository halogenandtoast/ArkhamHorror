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
encyclopedia uuid = Encyclopedia $ baseAttrs uuid "60208" $ slots .= [HandSlot]

instance HasModifiersFor env Encyclopedia where
  getModifiersFor _ _ _ = pure []

instance HasActions env Encyclopedia where
  getActions iid NonFast (Encyclopedia a) | ownedBy a iid = pure
    [ ActivateCardAbilityAction
        iid
        (mkAbility (toSource a) 1 (ActionAbility 1 Nothing))
    | not (assetExhausted a) && useCount (assetUses a) > 0
    ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Encyclopedia where
  runMessage msg a@(Encyclopedia attrs) = case msg of
    InvestigatorPlayAsset _ aid _ _ | aid == assetId attrs ->
      Encyclopedia <$> runMessage msg (attrs & uses .~ Uses Resource.Secret 5)
    EndPhase -> a <$ unshiftMessage (RemoveAllModifiersFrom (toSource attrs))
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      locationId <- asks $ getId @LocationId iid
      investigatorTargets <-
        asks $ map InvestigatorTarget . setToList . getSet locationId
      unshiftMessage $ chooseOne
        iid
        [ TargetLabel
            target
            [ chooseOne
                iid
                [ Label
                  "Willpower"
                  [AddModifiers target source [SkillModifier SkillWillpower 2]]
                , Label
                  "Intellect"
                  [AddModifiers target source [SkillModifier SkillIntellect 2]]
                , Label
                  "Combat"
                  [AddModifiers target source [SkillModifier SkillCombat 2]]
                , Label
                  "Agility"
                  [AddModifiers target source [SkillModifier SkillAgility 2]]
                ]
            ]
        | target <- investigatorTargets
        ]
      pure $ Encyclopedia $ attrs & exhausted .~ True & uses %~ Resource.use
    _ -> Encyclopedia <$> runMessage msg attrs
