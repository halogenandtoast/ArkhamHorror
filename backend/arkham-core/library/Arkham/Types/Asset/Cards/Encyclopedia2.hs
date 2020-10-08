{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.Encyclopedia2 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype Encyclopedia2 = Encyclopedia2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

encyclopedia2 :: AssetId -> Encyclopedia2
encyclopedia2 uuid =
  Encyclopedia2 $ (baseAttrs uuid "01042") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator Encyclopedia2 where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator Encyclopedia2 where
  getActions i NonFast (Encyclopedia2 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | not assetExhausted
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env Encyclopedia2 where
  runMessage msg a@(Encyclopedia2 attrs@Attrs {..}) = case msg of
    EndMythos ->
      a <$ unshiftMessage (RemoveAllModifiersFrom (AssetSource assetId))
    EndInvestigation ->
      a <$ unshiftMessage (RemoveAllModifiersFrom (AssetSource assetId))
    EndEnemy ->
      a <$ unshiftMessage (RemoveAllModifiersFrom (AssetSource assetId))
    EndUpkeep ->
      a <$ unshiftMessage (RemoveAllModifiersFrom (AssetSource assetId))
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      locationId <- asks (getId @LocationId iid)
      investigatorIds <- HashSet.toList <$> asks (getSet locationId)
      unshiftMessage
        (Ask iid $ ChooseOne
          [ TargetLabel
              (InvestigatorTarget iid')
              [ Ask
                  iid
                  (ChooseOne
                    [ Label
                      "Willpower"
                      [ AddModifiers
                          (InvestigatorTarget iid')
                          (AssetSource assetId)
                          [SkillModifier SkillWillpower 2]
                      ]
                    , Label
                      "Intellect"
                      [ AddModifiers
                          (InvestigatorTarget iid')
                          (AssetSource assetId)
                          [SkillModifier SkillIntellect 2]
                      ]
                    , Label
                      "Combat"
                      [ AddModifiers
                          (InvestigatorTarget iid')
                          (AssetSource assetId)
                          [SkillModifier SkillCombat 2]
                      ]
                    , Label
                      "Agility"
                      [ AddModifiers
                          (InvestigatorTarget iid')
                          (AssetSource assetId)
                          [SkillModifier SkillAgility 2]
                      ]
                    ]
                  )
              ]
          | iid' <- investigatorIds
          ]
        )
      pure $ Encyclopedia2 $ attrs & exhausted .~ True
    _ -> Encyclopedia2 <$> runMessage msg attrs
