{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Asset.Cards.ResearchLibrarian where

import Arkham.Json
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.AssetId
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude

newtype ResearchLibrarian = ResearchLibrarian Attrs
  deriving newtype (Show, ToJSON, FromJSON)

researchLibrarian :: AssetId -> ResearchLibrarian
researchLibrarian uuid = ResearchLibrarian $ (baseAttrs uuid "01032")
  { assetSlots = [AllySlot]
  , assetHealth = Just 1
  , assetSanity = Just 1
  }

instance HasModifiersFor env investigator ResearchLibrarian where
  getModifiersFor _ _ = pure []

instance HasActions env investigator ResearchLibrarian where
  getActions i window (ResearchLibrarian x) = getActions i window x

instance (AssetRunner env) => RunMessage env ResearchLibrarian where
  runMessage msg a@(ResearchLibrarian attrs@Attrs {..}) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId -> do
      unshiftMessage
        (Ask iid $ ChooseOne
          [ UseCardAbility
            iid
            (AssetSource assetId)
            (AssetSource assetId)
            Nothing
            1
          , Continue "Do not use ability"
          ]
        )
      ResearchLibrarian <$> runMessage msg attrs
    UseCardAbility iid _ (AssetSource aid) _ 1 | aid == assetId ->
      a <$ unshiftMessage
        (SearchDeckForTraits iid (InvestigatorTarget iid) [Tome])
    _ -> ResearchLibrarian <$> runMessage msg attrs
