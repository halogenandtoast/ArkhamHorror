{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.BookOfShadows3 where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import qualified Data.HashSet as HashSet
import Lens.Micro

newtype BookOfShadows3 = BookOfShadows3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

bookOfShadows3 :: AssetId -> BookOfShadows3
bookOfShadows3 uuid =
  BookOfShadows3 $ (baseAttrs uuid "01070") { assetSlots = [HandSlot] }

instance HasModifiersFor env investigator BookOfShadows3 where
  getModifiersFor _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator BookOfShadows3 where
  getActions i NonFast (BookOfShadows3 Attrs {..})
    | Just (getId () i) == assetInvestigator = pure
      [ ActivateCardAbilityAction
          (getId () i)
          (mkAbility (AssetSource assetId) 1 (ActionAbility 1 Nothing))
      | not assetExhausted
      ]
  getActions _ _ _ = pure []

instance (AssetRunner env) => RunMessage env BookOfShadows3 where
  runMessage msg (BookOfShadows3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage (AddSlot iid ArcaneSlot (Slot (AssetSource aid) Nothing))
      BookOfShadows3 <$> runMessage msg attrs
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId -> do
      assetIds <- HashSet.toList <$> asks (getSet iid)
      spellAssetIds <- flip filterM assetIds $ \aid' -> do
        traits <- asks (getSet aid')
        pure $ Spell `member` traits
      unless (null spellAssetIds) $ unshiftMessage
        (Ask iid $ ChooseOne
          [ AddUses (AssetTarget aid') Charge 1 | aid' <- spellAssetIds ]
        )
      pure $ BookOfShadows3 $ attrs & exhausted .~ True
    _ -> BookOfShadows3 <$> runMessage msg attrs
