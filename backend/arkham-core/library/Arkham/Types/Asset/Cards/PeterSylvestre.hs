{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PeterSylvestre where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype PeterSylvestre = PeterSylvestre Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterSylvestre :: AssetId -> PeterSylvestre
peterSylvestre uuid = PeterSylvestre $ (baseAttrs uuid "02033")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 2
  }

instance IsInvestigator investigator => HasModifiersFor env investigator PeterSylvestre where
  getModifiersFor _ i (PeterSylvestre a) =
    pure [ SkillModifier SkillAgility 1 | ownedBy a i ]

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility (AfterEndTurn You))

instance (ActionRunner env investigator) => HasActions env investigator PeterSylvestre where
  getActions i (AfterEndTurn You) (PeterSylvestre a) | ownedBy a i = do
    let ability' = (getId () i, ability a)
    unused <- asks $ notElem ability' . map unUsedAbility . getList ()
    pure
      [ uncurry ActivateCardAbilityAction ability'
      | unused && assetSanityDamage a > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre where
  runMessage msg (PeterSylvestre attrs) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source ->
      pure $ PeterSylvestre $ attrs & sanityDamage -~ 1
    _ -> PeterSylvestre <$> runMessage msg attrs
