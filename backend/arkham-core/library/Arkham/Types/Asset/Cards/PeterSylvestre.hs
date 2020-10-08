{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PeterSylvestre where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Window
import ClassyPrelude
import Lens.Micro

newtype PeterSylvestre = PeterSylvestre Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterSylvestre :: AssetId -> PeterSylvestre
peterSylvestre uuid = PeterSylvestre $ (baseAttrs uuid "02033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator PeterSylvestre where
  getModifiersFor _ i (PeterSylvestre Attrs {..}) = pure
    [ SkillModifier SkillAgility 1 | Just (getId () i) == assetInvestigator ]

instance (ActionRunner env investigator) => HasActions env investigator PeterSylvestre where
  getActions i (AfterEndTurn You) (PeterSylvestre Attrs {..})
    | Just (getId () i) == assetInvestigator = do
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      let
        ability =
          mkAbility (AssetSource assetId) 1 (ReactionAbility (AfterEndTurn You))
      pure
        [ ActivateCardAbilityAction (getId () i) ability
        | (getId () i, ability) `notElem` usedAbilities && assetSanityDamage > 0
        ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre where
  runMessage msg (PeterSylvestre attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (AssetSource aid) _ 1 | aid == assetId -> do
      pure $ PeterSylvestre $ attrs & sanityDamage -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
