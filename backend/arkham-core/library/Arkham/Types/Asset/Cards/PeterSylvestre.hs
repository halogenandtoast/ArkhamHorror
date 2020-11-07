{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.PeterSylvestre
  ( PeterSylvestre(..)
  , peterSylvestre
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner

newtype PeterSylvestre = PeterSylvestre Attrs
  deriving newtype (Show, ToJSON, FromJSON)

peterSylvestre :: AssetId -> PeterSylvestre
peterSylvestre uuid = PeterSylvestre $ baseAttrs uuid "02033" $ do
  slots .= [AllySlot]
  health ?= 1
  sanity ?= 2

instance HasModifiersFor env PeterSylvestre where
  getModifiersFor _ (InvestigatorTarget iid) (PeterSylvestre a) =
    pure [ SkillModifier SkillAgility 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ReactionAbility (AfterEndTurn You))

instance ActionRunner env => HasActions env PeterSylvestre where
  getActions iid (AfterEndTurn You) (PeterSylvestre a) | ownedBy a iid = do
    let ability' = (iid, ability a)
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
