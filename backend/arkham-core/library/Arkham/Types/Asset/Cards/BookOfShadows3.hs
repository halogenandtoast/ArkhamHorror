{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BookOfShadows3
  ( BookOfShadows3(..)
  , bookOfShadows3
  )
where

import Arkham.Import

import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.Trait

newtype BookOfShadows3 = BookOfShadows3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bookOfShadows3 :: AssetId -> BookOfShadows3
bookOfShadows3 uuid =
  BookOfShadows3 $ baseAttrs uuid "01070" $ slots .= [HandSlot]

instance HasModifiersFor env BookOfShadows3 where
  getModifiersFor _ _ _ = pure []

ability :: Attrs -> Ability
ability a = mkAbility (toSource a) 1 (ActionAbility 1 Nothing)

slot :: Attrs -> Slot
slot Attrs { assetId } = Slot (AssetSource assetId) Nothing

instance HasActions env BookOfShadows3 where
  getActions iid NonFast (BookOfShadows3 a) | ownedBy a iid =
    pure [ ActivateCardAbilityAction iid (ability a) | not (assetExhausted a) ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BookOfShadows3 where
  runMessage msg (BookOfShadows3 attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      unshiftMessage (AddSlot iid ArcaneSlot (slot attrs))
      BookOfShadows3 <$> runMessage msg attrs
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      assetIds <- asks $ setToList . getSet iid
      spellAssetIds <- filterM (asks . (member Spell .) . getSet) assetIds
      unless
        (null spellAssetIds)
        (unshiftMessage $ chooseOne
          iid
          [ AddUses (AssetTarget aid') Charge 1 | aid' <- spellAssetIds ]
        )
      pure $ BookOfShadows3 $ attrs & exhausted .~ True
    _ -> BookOfShadows3 <$> runMessage msg attrs
