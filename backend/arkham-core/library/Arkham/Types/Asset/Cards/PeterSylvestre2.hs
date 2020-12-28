{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PeterSylvestre2
  ( PeterSylvestre2(..)
  , peterSylvestre2
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner

newtype PeterSylvestre2 = PeterSylvestre2 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterSylvestre2 :: AssetId -> PeterSylvestre2
peterSylvestre2 uuid = PeterSylvestre2 $ (baseAttrs uuid "02035")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 3
  }

instance HasModifiersFor env PeterSylvestre2 where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre2 a)
    | ownedBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillAgility 1, SkillModifier SkillWillpower 1]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility (AfterEndTurn You))

instance ActionRunner env => HasActions env PeterSylvestre2 where
  getActions iid (AfterEndTurn You) (PeterSylvestre2 a) | ownedBy a iid = do
    let ability' = (iid, ability a)
    unused <- notElem ability' . map unUsedAbility <$> getList ()
    pure
      [ uncurry ActivateCardAbilityAction ability'
      | unused && assetSanityDamage a > 0
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env PeterSylvestre2 where
  runMessage msg (PeterSylvestre2 attrs) = case msg of
    UseCardAbility _ source _ 1 | isSource attrs source ->
      pure $ PeterSylvestre2 $ attrs & sanityDamageL -~ 1
    _ -> PeterSylvestre2 <$> runMessage msg attrs
